{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Fugacious.Logger
    ( Config (..)
    , Handle
    , withHandle

    , error
    , info
    ) where

import           Control.Exception       (bracket)
import qualified Data.Aeson              as A
import           Data.Monoid             (Last (..))
import           Prelude                 hiding (error)
import qualified System.Log.FastLogger   as FL

data Config = Config
    { cPath :: Last FilePath
    } deriving (Show)

instance Monoid Config where
    mempty                      = Config mempty
    Config l `mappend` Config r = Config (l `mappend` r)

instance A.FromJSON Config where
    parseJSON = A.withObject "FromJSON Fugacious.Logger.Config" $ \o ->
        Config <$> o A..: "path"

data Handle = Handle
    { hLoggerSet :: FL.LoggerSet
    }

withHandle :: Config -> (Handle -> IO a) -> IO a
withHandle config f = bracket
    (case getLast (cPath config) of
        Nothing   -> FL.newStderrLoggerSet FL.defaultBufSize
        Just "-"  -> FL.newStderrLoggerSet FL.defaultBufSize
        Just path -> FL.newFileLoggerSet FL.defaultBufSize path)
    FL.rmLoggerSet
    (\l -> f Handle {hLoggerSet = l})

error :: FL.ToLogStr str => Handle -> str -> IO ()
error h = FL.pushLogStrLn (hLoggerSet h) . FL.toLogStr

info :: FL.ToLogStr str => Handle -> str -> IO ()
info h = FL.pushLogStrLn (hLoggerSet h) . FL.toLogStr
