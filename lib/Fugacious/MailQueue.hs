{-# LANGUAGE OverloadedStrings #-}
module Fugacious.MailQueue
    ( DeliverMail
    , Config (..)
    , Handle (..)
    , withHandle
    , withHandles
    ) where

import qualified Data.Aeson                   as A
import qualified Fugacious.Logger             as Logger
import qualified Fugacious.MailQueue.Amazon   as Amazon
import           Fugacious.MailQueue.Internal
import qualified Fugacious.MailQueue.Spool    as Spool

data Config
    = AmazonConfig Amazon.Config
    | SpoolConfig  Spool.Config

instance A.FromJSON Config where
    parseJSON = A.withObject "FromJSON Fugacious.MailQueue" $ \o -> do
        ty <- o A..: "type"
        case ty of
            "amazon" -> AmazonConfig <$> A.parseJSON (A.Object o)
            "spool"  -> SpoolConfig  <$> A.parseJSON (A.Object o)
            _        -> fail $ "Unknown MailQueue type: " ++ ty

withHandle :: Config -> Logger.Handle -> (Handle -> IO a) -> IO a
withHandle (AmazonConfig c) logger  = Amazon.withHandle c logger
withHandle (SpoolConfig  c) _logger = Spool.withHandle  c

withHandles :: [Config] -> Logger.Handle -> ([Handle] -> IO a) -> IO a
withHandles []       _logger f = f []
withHandles (c : cs) logger  f =
    withHandle  c  logger $ \h  ->
    withHandles cs logger $ \hs ->
        f (h : hs)
