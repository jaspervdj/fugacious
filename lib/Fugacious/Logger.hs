{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Fugacious.Logger
    ( Verbosity (..)
    , Config (..)
    , Handle
    , withHandle

    , debug
    , info
    , warning
    , error

    , debug'
    , info'
    , warning'
    , error'
    ) where

import           Control.Applicative   (Alternative (..))
import           Control.Exception     (bracket)
import qualified Data.Aeson            as A
import           Data.Maybe            (fromMaybe)
import qualified Data.Text             as T
import           Prelude               hiding (error, log)
import qualified System.Log.FastLogger as FL

data Verbosity
    = Debug
    | Info
    | Warning
    | Error
    deriving (Eq, Ord, Show)

instance A.FromJSON Verbosity where
    parseJSON = A.withText "FromJSON Fugacious.Logger.Verbosity" $ \t ->
        case t of
            "debug"   -> pure Debug
            "info"    -> pure Info
            "warning" -> pure Warning
            "error"   -> pure Error
            _         -> fail $ "Unknown verbosity: " ++ T.unpack t

data Config = Config
    { cPath      :: Maybe FilePath
    , cVerbosity :: Maybe Verbosity
    } deriving (Show)

instance Monoid Config where
    mempty                              = Config empty empty
    Config p0 v0 `mappend` Config p1 v1 = Config (p0 <|> p1) (v0 <|> v1)

instance A.FromJSON Config where
    parseJSON = A.withObject "FromJSON Fugacious.Logger.Config" $ \o -> Config
        <$> o A..:? "path"
        <*> o A..:? "verbosity"

data Handle = Handle
    { hConfig    :: Config
    , hLoggerSet :: FL.LoggerSet
    }

withHandle :: Config -> (Handle -> IO a) -> IO a
withHandle config f = bracket
    (case cPath config of
        Nothing   -> FL.newStderrLoggerSet FL.defaultBufSize
        Just "-"  -> FL.newStderrLoggerSet FL.defaultBufSize
        Just path -> FL.newFileLoggerSet FL.defaultBufSize path)
    FL.rmLoggerSet
    (\l -> f Handle {hConfig = config, hLoggerSet = l})

log :: FL.ToLogStr s => Handle -> Verbosity -> s -> IO ()
log Handle {..} v x
    | v >= verbosity = FL.pushLogStrLn hLoggerSet $ FL.toLogStr x
    | otherwise      = return ()
  where
    verbosity = fromMaybe Debug (cVerbosity hConfig)

debug, info, warning, error :: FL.ToLogStr str => Handle -> str -> IO ()
debug   h = log h Debug
info    h = log h Info
warning h = log h Warning
error   h = log h Error

debug', info', warning', error' :: Handle -> String -> IO ()
debug'   = debug
info'    = info
warning' = warning
error'   = error
