-- | This is a module that makes a "let's try our best" attempt at parsing
-- mails.  Mail formats get ridiculously complicated but we can come up with
-- something that works 90% of the time.
--
-- TODO: A problem is that we receive the mail as 'T.Text', whereas it should
-- really be a bytestring.  This is both bad for performance (many encode/decode
-- trips), as well as correctness (non UTF-8 mails).
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Fugacious.ParseMail
    ( ParsedMail (..)
    , MailBody (..)
    , parseMail
    ) where

import qualified Data.ByteString.Base64.Lazy as Base64
import           Data.Char                   (isSpace)
import           Data.Either                 (partitionEithers)
import           Data.Either.Fail
import qualified Data.MBox                   as MBox
import qualified Data.Text                   as T
import qualified Data.Text.Lazy              as TL
import qualified Data.Text.Lazy.Encoding     as TL
import           Debug.Trace
import qualified Network.Multipart           as Multipart

data ParsedMail = ParsedMail
    { pmSubject :: !T.Text
    , pmFrom    :: !T.Text
    , pmTo      :: !T.Text
    , pmBody    ::  MailBody
    } deriving (Show)

data MailBody
    = PlainTextBody T.Text
    | HtmlBody      T.Text
    | MultipartBody [MailBody]
    deriving (Show)

parseMail :: T.Text -> Either String ParsedMail
parseMail source = do
    msg       <- case MBox.parseMBox (TL.fromStrict source) of
        [msg] -> Right msg
        _     -> Left "Could not parse mail source"
    pmFrom    <- getHeader "From"    msg
    pmTo      <- getHeader "To"      msg
    pmSubject <- getHeader "Subject" msg
    pmBody    <- parseMailBody (headers msg) (MBox.body msg)
    return ParsedMail {..}
  where
    lookupHeader key =
        fmap TL.toStrict . lookup (TL.fromStrict key) . MBox.headers

    getHeader :: T.Text -> MBox.Message -> Either String T.Text
    getHeader key msg = case lookupHeader key msg of
        Nothing  -> Left $ "Missing header: " ++ show key
        Just val -> Right val

    headers :: MBox.Message -> [(Multipart.HeaderName, String)]
    headers msg =
        [ (Multipart.HeaderName (TL.unpack k), TL.unpack v)
        | (k, v) <- MBox.headers msg
        ]

parseMailBody
    :: [(Multipart.HeaderName, String)]  -- ^ Headers
    -> TL.Text                           -- ^ Body
    -> Either String MailBody
parseMailBody headers ebody
    -- No content type
    | Nothing <- contentType = do
        dbody <- contentTransferDecode ebody
        return $ PlainTextBody $ TL.toStrict dbody

    -- Multipart mails
    | Just (Right Multipart.ContentType {..}) <- contentType
            , ctType == "multipart"
            , Just boundary <- lookup "boundary" ctParameters = do
        dbody <- contentTransferDecode ebody
        let Multipart.MultiPart parts =
                Multipart.parseMultipartBody boundary (TL.encodeUtf8 dbody)
        let trace' x = traceShow x x
        return $ MultipartBody $ snd $ trace' $ partitionEithers
            [ parseMailBody pheaders (TL.decodeUtf8 pbody)
            | Multipart.BodyPart pheaders pbody <- parts
            ]

    -- Explicit text/plain mails
    | Just (Right Multipart.ContentType {..}) <- contentType
            , ctType == "text" && ctSubtype == "plain" = do
        dbody <- contentTransferDecode ebody
        return $ PlainTextBody $ TL.toStrict dbody

    -- Explicit text/html mails
    | Just (Right Multipart.ContentType {..}) <- contentType
            , ctType == "text" && ctSubtype == "html" = do
        dbody <- contentTransferDecode ebody
        return $ HtmlBody $ TL.toStrict dbody

    -- Everything else
    | Just (Right ct) <- contentType = Left $ "Unsupported Content-Type" ++ show ct
    | Just (Left err) <- contentType = Left err
  where
    contentType :: Maybe (Either String Multipart.ContentType)
    contentType =
        fmap (runFail . Multipart.parseContentType) $
        lookup (Multipart.HeaderName "Content-Type") headers

    contentTransferEncoding :: Maybe String
    contentTransferEncoding =
        lookup (Multipart.HeaderName "Content-Transfer-Encoding") headers

    contentTransferDecode :: TL.Text -> Either String TL.Text
    contentTransferDecode plain = case contentTransferEncoding of
        Nothing       -> Right plain
        Just "base64" ->
            fmap TL.decodeUtf8 $ Base64.decode $ TL.encodeUtf8 $
            TL.filter (not . isSpace) plain
        Just _        -> Left "Unsupported Content-Transfer-Encoding"
