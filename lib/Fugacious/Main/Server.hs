{-# LANGUAGE OverloadedStrings #-}
module Fugacious.Main.Server
    ( Config (..)

    , main
    ) where

import           Control.Concurrent         (threadDelay)
import qualified Control.Concurrent.Async   as Async
import           Control.Monad              (forM_, forever, void)
import qualified Data.Aeson                 as A
import           Data.Monoid                ((<>))
import           Data.Version               (showVersion)
import qualified Data.Yaml                  as Yaml
import qualified Fugacious.Database         as Database
import qualified Fugacious.Logger           as Logger
import qualified Fugacious.MailQueue        as MailQueue
import           Fugacious.ParseMail
import qualified Fugacious.Web              as Web
import qualified Paths_fugacious
import           System.Environment         (getArgs, getProgName)
import           System.Exit                (exitFailure)
import qualified System.IO                  as IO

data Config = Config
    { cLogger     :: Logger.Config
    , cWeb        :: Web.Config
    , cDatabase   :: Database.Config
    , cMailQueues :: [MailQueue.Config]
    }

instance Monoid Config where
    mempty = Config
        { cLogger     = mempty
        , cWeb        = mempty
        , cDatabase   = mempty
        , cMailQueues = mempty
        }

    mappend l r = Config
        { cLogger     = cLogger     l <> cLogger     r
        , cWeb        = cWeb        l <> cWeb        r
        , cDatabase   = cDatabase   l <> cDatabase   r
        , cMailQueues = cMailQueues l <> cMailQueues r
        }

instance A.FromJSON Config where
    parseJSON = A.withObject "FromJSON Fugacious.Main.Server.Config" $ \o ->
        Config
            <$> o A..:? "logger"      A..!= mempty
            <*> o A..:? "web"         A..!= mempty
            <*> o A..:? "database"    A..!= mempty
            <*> o A..:? "mail_queues" A..!= mempty

main :: IO ()
main = do
    args     <- getArgs
    progName <- getProgName
    case args of
        [configPath] -> run configPath
        _ -> do
            IO.hPutStrLn IO.stderr $ "Usage: " ++ progName ++ " <conf>"
            exitFailure

run :: FilePath -> IO ()
run configPath = do
    IO.hPutStrLn IO.stderr $
        "Booting fugacious v" ++ showVersion Paths_fugacious.version

    errOrConfig <- Yaml.decodeFileEither configPath
    config      <- either (fail . show) return errOrConfig

    let loggerConfig  = cLogger     config
        dbConfig      = cDatabase   config
        webConfig     = cWeb        config
        queueConfigs  = cMailQueues config

    Logger.withHandle loggerConfig $ \logger ->
        Database.withHandle dbConfig $ \db ->
        Web.withHandle webConfig logger db $ \web ->
        MailQueue.withHandles queueConfigs logger $ \mailQueues ->
        Async.withAsync (popThread logger db mailQueues) $ \_ ->
        Async.withAsync (janitorThread logger db) $ \_ ->
            Web.run web

popThread :: Logger.Handle -> Database.Handle -> [MailQueue.Handle] -> IO ()
popThread logger db queues = forever $ do
    threadDelay $ 30 * 1000 * 1000
    forM_ queues $ \queue ->
        gracefully logger "Could not pop queue" $
        MailQueue.pop queue $ \source -> do
            Logger.debug' logger "Popped an email."
            case parseMail source of
                Left err -> Logger.error logger err
                Right pm -> gracefully logger "Could not deliver mail" $ do
                    Logger.info logger $ "Delivering to: " ++ show (pmTo pm)
                    void $ Database.deliverMail
                        db (pmFrom pm) (pmTo pm) (pmSubject pm) source

janitorThread :: Logger.Handle -> Database.Handle -> IO ()
janitorThread logger db = forever $ do
    threadDelay $ 60 * 1000 * 1000
    expired <- Database.getExpiredUsers db
    forM_ expired $ \user -> do
        Logger.debug logger $ "Purging user " <> Database.uAddress user
        Database.purgeUser db user

gracefully :: Logger.Handle -> String -> IO () -> IO ()
gracefully logger prefix action = do
    errOrUnit <- Async.withAsync action Async.waitCatch
    case errOrUnit of
        Left err -> Logger.error logger $ prefix ++ ": " ++ (show err)
        Right () -> return ()
