{-# LANGUAGE OverloadedStrings #-}
module Snap.Core.Extended
    ( module Snap.Core

    , FromParam (..)
    , requireParam

    , throw400
    , throw500
    , throwError

    , plainText
    ) where

import qualified Data.ByteString    as B
import           Data.Int           (Int64)
import qualified Data.Text          as T
import qualified Data.Text.Encoding as T
import           Snap.Core
import           Text.Read          (readMaybe)

class FromParam a where
    fromParam :: B.ByteString -> Maybe a

instance FromParam T.Text where
    fromParam = Just . T.decodeUtf8

instance FromParam Int64 where
    fromParam = readMaybe . T.unpack . T.decodeUtf8

requireParam :: (FromParam a, MonadSnap m) => B.ByteString -> m a
requireParam key = do
    mbBs <- getParam key
    case mbBs of
        Nothing -> throw400 $ "Missing required parameter: " ++ show key
        Just bs -> case fromParam bs of
            Nothing -> throw400 $ "Error decoding parameter: " ++ show key
            Just x  -> return x

throw400 :: MonadSnap m => String -> m a
throw400 = throwError 400 "User Error"

throw500 :: MonadSnap m => String -> m a
throw500 = throwError 500 "Server Error"

throwError :: MonadSnap m => Int -> String -> String -> m a
throwError code message reason = do
    modifyResponse $ setResponseStatus code $ T.encodeUtf8 $ T.pack message
    writeBS $ T.encodeUtf8 $ T.pack $
        show code ++ " " ++ message ++ ": " ++ reason
    r <- getResponse
    finishWith r

plainText :: MonadSnap m => T.Text -> m ()
plainText text = do
    modifyResponse $ addHeader "Content-Type" "text/plain; charset=UTF-8"
    writeBS $ T.encodeUtf8 text
