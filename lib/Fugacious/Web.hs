{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Fugacious.Web
    ( Config (..)

    , Handle (..)
    , withHandle
    , run
    ) where

import           Control.Exception.Lifted (Handler (..), catches)
import           Control.Monad.Reader     (ReaderT, asks, runReaderT)
import           Control.Monad.Trans      (liftIO)
import qualified Data.Aeson               as A
import           Data.Maybe               (fromMaybe)
import           Data.Monoid              (Last (..), (<>))
import qualified Data.Text                as T
import qualified Data.Text.Encoding       as T
import qualified Data.Time                as Time
import qualified Fugacious.Database       as Database
import qualified Fugacious.Logger         as Logger
import qualified Fugacious.ParseMail      as ParseMail
import qualified Fugacious.Web.Views      as Views
import qualified Snap.Blaze               as Snap
import           Snap.Core.Extended       (MonadSnap, Snap)
import qualified Snap.Core.Extended       as Snap
import qualified Snap.Http.Server         as Snap
import qualified Snap.Util.FileServe      as Snap

data Config = Config
    { cPort         :: Last Int
    , cMailDomain   :: Last T.Text
    , cWebDomain    :: Last T.Text
    , cUserLifetime :: Last Int
    } deriving (Show)

instance Monoid Config where
    mempty        = Config mempty mempty mempty mempty
    mappend c1 c2 = Config
        { cPort         = cPort         c1 <> cPort         c2
        , cMailDomain   = cMailDomain   c1 <> cMailDomain   c2
        , cWebDomain    = cWebDomain    c1 <> cWebDomain    c2
        , cUserLifetime = cUserLifetime c1 <> cUserLifetime c2
        }

instance A.FromJSON Config where
    parseJSON = A.withObject "FromJSON Fugacious.Web.Config" $ \o -> Config
        <$> o A..: "port"
        <*> o A..: "mail_domain"
        <*> o A..: "web_domain"
        <*> o A..: "user_lifetime"

data Handle = Handle
    { hConfig   :: Config
    , hLogger   :: Logger.Handle
    , hDatabase :: Database.Handle
    }

type FugaciousM = ReaderT Handle Snap

withHandle
    :: Config -> Logger.Handle -> Database.Handle -> (Handle -> IO a) -> IO a
withHandle config logger database f =
    f $ Handle config logger database

askMailDomain :: FugaciousM T.Text
askMailDomain = asks $ fromMaybe "localhost" . getLast . cMailDomain . hConfig

askWebDomain :: FugaciousM T.Text
askWebDomain = asks $ fromMaybe "localhost" . getLast . cWebDomain . hConfig

askUserLifetime :: FugaciousM Int
askUserLifetime = asks $ fromMaybe 600 . getLast . cUserLifetime . hConfig

run :: Handle -> IO ()
run h =
    Snap.httpServe sconf (runReaderT app h)
  where
    port  = fromMaybe 8000 $ getLast $ cPort $ hConfig h
    sconf =
        Snap.setPort port $
        Snap.setAccessLog Snap.ConfigNoLog $
        Snap.setErrorLog (Snap.ConfigIoLog (Logger.error (hLogger h))) $
        Snap.defaultConfig

app :: FugaciousM ()
app = Snap.route
    [ ("",                    Snap.ifTop index)
    , ("/users",              Snap.method Snap.POST postUsers)
    , ("/users/:user/logout", Snap.method Snap.POST postUsersLogout)
    , ("/users/:user/renew",  Snap.method Snap.POST postUsersRenew)
    , ("/inbox/:user",        Snap.ifTop getInbox)
    , ("/inbox/:user/:mail",  getInboxMail)
    , ("/assets/style.css",   Snap.serveFile "assets/style.css")
    ]

index :: FugaciousM ()
index = handleExceptions $ do
    mailDomain <- askMailDomain
    Snap.blaze $ Views.index mailDomain

expirationFromNow :: FugaciousM Time.UTCTime
expirationFromNow = do
    lifetime   <- askUserLifetime
    now <- liftIO Time.getCurrentTime
    return $ Time.addUTCTime (fromIntegral lifetime) now

postUsers :: FugaciousM ()
postUsers = handleExceptions $ do
    address    <- Snap.requireParam "address"
    webDomain  <- askWebDomain
    mailDomain <- askMailDomain
    db         <- asks hDatabase

    expires <- expirationFromNow
    user    <- liftIO $ Database.createUser db (address <> "@" <> mailDomain) expires

    let cookie = Snap.Cookie
            { Snap.cookieName     = "user"
            , Snap.cookieValue    = T.encodeUtf8 (Database.uToken user)
            , Snap.cookieExpires  = Nothing
            , Snap.cookieDomain   = Just (T.encodeUtf8 webDomain)
            , Snap.cookiePath     = Just "/"
            , Snap.cookieSecure   = False
            , Snap.cookieHttpOnly = False
            }

    Snap.modifyResponse $ Snap.addResponseCookie cookie
    Snap.redirect $ "/inbox/" <> T.encodeUtf8 (Database.uId user)

postUsersLogout :: FugaciousM ()
postUsersLogout = handleExceptions $ do
    user   <- authorize
    db     <- asks hDatabase
    liftIO $ Database.purgeUser db user
    Snap.redirect "/"

postUsersRenew :: FugaciousM ()
postUsersRenew = handleExceptions $ do
    user    <- authorize
    db      <- asks hDatabase
    expires <- expirationFromNow
    liftIO $ Database.renewUser db user expires
    Snap.redirect $ "/inbox/" <> T.encodeUtf8 (Database.uId user)

getInbox :: FugaciousM ()
getInbox = handleExceptions $ do
    user   <- authorize
    db     <- asks hDatabase
    mails  <- liftIO $ Database.getMailByUser db user
    now    <- liftIO $ Time.getCurrentTime
    Snap.blaze $ Views.inbox now user mails

getInboxMail :: FugaciousM ()
getInboxMail = handleExceptions $ do
    user  <- authorize
    id_   <- Snap.requireParam "mail"
    db    <- asks hDatabase
    now   <- liftIO $ Time.getCurrentTime
    mail  <- liftIO $ Database.getMailById db id_

    sourceParam <- Snap.getParam "source"
    case sourceParam of
        Just _  -> Snap.plainText $ Database.mSource mail
        Nothing -> do
            pmail <- case ParseMail.parseMail (Database.mSource mail) of
                Left  err -> Snap.throw500 $ "Could not parser mail: " ++ err
                Right x   -> return x
            Snap.blaze $ Views.mail now user (Database.mId mail) pmail

authorize :: FugaciousM Database.User
authorize = do
    id_      <- Snap.requireParam "user"
    db       <- asks hDatabase
    user     <- liftIO $ Database.getUserById db id_
    mbCookie <- Snap.getCookie "user"

    case mbCookie of
        Nothing -> Snap.throw400 "No user cookie"
        Just Snap.Cookie {..}
            | cookieValue /= T.encodeUtf8 (Database.uToken user) ->
                Snap.throw400 "Invalid cookie"
            | otherwise -> return user

-- | This function catches a variety of exceptions that can be thrown and turns
-- them into nice HTTP/HTML responses.
handleExceptions :: MonadSnap m => m a -> m a
handleExceptions action = catches action
    [ Handler $ \db -> case db of
        Database.Constraint err -> Snap.throwError 400 "Constraint Error" err
        Database.NotFound   err -> Snap.throwError 404 "Not Found"        err
    ]
