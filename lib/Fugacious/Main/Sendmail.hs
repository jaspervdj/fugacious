{-# LANGUAGE OverloadedStrings #-}
module Fugacious.Main.Sendmail
    ( main
    ) where

import qualified Data.MBox.Extended        as MBox
import qualified Data.Text                 as T
import qualified Data.Text.IO              as T
import qualified Data.Text.Lazy            as TL
import qualified Fugacious.MailQueue.Spool as Spool
import           System.Environment        (getArgs, getProgName)
import           System.Exit               (exitFailure)
import qualified System.IO                 as IO

prompt :: String -> IO T.Text
prompt msg = do
    putStr $ msg ++ ": "
    IO.hFlush IO.stdout
    T.getLine

main :: IO ()
main = do
    args     <- getArgs
    progName <- getProgName
    case args of
        [spoolPath] -> run spoolPath
        _ -> do
            IO.hPutStrLn IO.stderr $ "Usage: " ++ progName ++ " <spool>"
            exitFailure

run :: FilePath -> IO ()
run spoolPath = do
    let spoolConfig = Spool.Config {Spool.cPath = spoolPath}

    from    <- prompt "From"
    to      <- prompt "To"
    subject <- prompt "Subject"
    putStrLn "Body:"
    body <- T.getContents
    message <- MBox.makeMessage from to subject body
    putStrLn "Sending..."
    Spool.send spoolConfig $ TL.toStrict $ MBox.showMessage message
    putStrLn "Looks like I'm done"
