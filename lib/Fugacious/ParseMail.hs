{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Fugacious.ParseMail
    ( ParsedMail (..)
    , parseMail
    ) where

import qualified Data.MBox      as MBox
import qualified Data.Text      as T
import qualified Data.Text.Lazy as TL

data ParsedMail = ParsedMail
    { pmSubject :: !T.Text
    , pmFrom    :: !T.Text
    , pmTo      :: !T.Text
    , pmBody    :: !T.Text
    } deriving (Show)

parseMail :: T.Text -> Either String ParsedMail
parseMail source = do
    msg       <- case MBox.parseMBox (TL.fromStrict source) of
        [msg] -> Right msg
        _     -> Left "Could not parse mail source"
    pmFrom    <- getHeader "From"    msg
    pmTo      <- getHeader "To"      msg
    pmSubject <- getHeader "Subject" msg
    let pmBody = TL.toStrict (MBox.body msg)
    return ParsedMail {..}
  where
    getHeader :: T.Text -> MBox.Message -> Either String T.Text
    getHeader key msg = case lookup (TL.fromStrict key) (MBox.headers msg) of
        Nothing  -> Left $ "Missing header: " ++ show key
        Just val -> Right $ TL.toStrict val
