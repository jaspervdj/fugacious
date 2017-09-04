{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Fugacious.MailQueue.Spool
    ( Config (..)

    , Handle (..)
    , withHandle

    , send
    ) where

import           Control.Monad                (forM_)
import qualified Data.Aeson                   as A
import qualified Data.MBox                    as MBox
import           Data.Monoid                  ((<>))
import qualified Data.Text                    as T
import qualified Data.Text.IO                 as T
import qualified Data.Text.Lazy               as TL
import qualified Data.Text.Lazy.IO            as TL
import           Fugacious.MailQueue.Internal
import qualified System.FileLock.Extended     as FileLock

data Config = Config
    { cPath :: FilePath
    } deriving (Show)

instance A.FromJSON Config where
    parseJSON = A.withObject "FromJSON Fugacious.MailQueue.Spool.Config" $ \o ->
        Config <$> o A..: "path"

withHandle :: Config -> (Handle -> IO a) -> IO a
withHandle config f = f Handle
    { pop = popWith config
    }

popWith :: Config -> DeliverMail -> IO ()
popWith config deliver = FileLock.simpleFileLock spoolFile $ do
    contents <- TL.readFile spoolFile

    -- Make the file is fully read.
    TL.length contents `seq` return ()

    forM_ (MBox.parseMBox contents) $ \msg -> do
        deliver $ TL.toStrict $ MBox.showMessage $ msg

    TL.writeFile spoolFile ""
  where
    spoolFile = cPath config

send :: Config -> T.Text -> IO ()
send config mail = FileLock.simpleFileLock spoolFile $ do
    !contents <- T.readFile spoolFile
    T.writeFile spoolFile $
        if T.null contents
            then mail
            else contents <> "\n\n" <> mail
  where
    spoolFile = cPath config
