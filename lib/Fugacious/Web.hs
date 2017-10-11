{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Fugacious.Web
    ( Config (..)

    , Handle (..)
    , withHandle
    , run
    ) where

import           Control.Applicative      (empty, (<|>))
import           Control.Exception.Lifted (Handler (..), catches)
import           Control.Monad.Reader     (ReaderT, asks, runReaderT)
import           Control.Monad.Trans      (liftIO)
import qualified Data.Aeson               as A
import           Data.Maybe               (fromMaybe)
import           Data.Monoid              ((<>))
import qualified Data.Text                as T
import qualified Data.Text.Encoding       as T
import qualified Data.Time                as Time
import qualified Fugacious.Database       as Database
import qualified Fugacious.Logger         as Logger
import qualified Fugacious.ParseMail      as ParseMail
import qualified Fugacious.User           as User
import           Fugacious.Web.Routes     (Route (..))
import qualified Fugacious.Web.Routes     as Routes
import qualified Fugacious.Web.Views      as Views
import qualified Snap.Blaze               as Snap
import           Snap.Core.Extended       (MonadSnap, Snap)
import qualified Snap.Core.Extended       as Snap
import qualified Snap.Http.Server         as Snap
import qualified Snap.Util.FileServe      as Snap

data Config = Config
    { cPort   :: Maybe Int
    , cDomain :: Maybe T.Text
    } deriving (Show)

instance Monoid Config where
    mempty        = Config empty empty
    mappend c1 c2 = Config
        { cPort   = cPort   c1 <|> cPort   c2
        , cDomain = cDomain c1 <|> cDomain c2
        }

instance A.FromJSON Config where
    parseJSON = A.withObject "FromJSON Fugacious.Web.Config" $ \o -> Config
        <$> o A..:? "port"
        <*> o A..:? "domain"

data Handle = Handle
    { hConfig   :: Config
    , hUser     :: User.Config
    , hLogger   :: Logger.Handle
    , hDatabase :: Database.Handle
    }

type FugaciousM = ReaderT Handle Snap

withHandle
    :: Config -> User.Config -> Logger.Handle -> Database.Handle
    -> (Handle -> IO a) -> IO a
withHandle config userConfig logger database f =
    f $ Handle config userConfig logger database

askMailDomain :: FugaciousM T.Text
askMailDomain = asks $ fromMaybe "localhost" . User.cDomain . hUser

askWebDomain :: FugaciousM T.Text
askWebDomain = asks $ fromMaybe "localhost" . cDomain . hConfig

askUserLifetime :: FugaciousM Int
askUserLifetime = asks $ fromMaybe 600 . User.cLifetime . hUser

run :: Handle -> IO ()
run h =
    Snap.httpServe sconf (runReaderT app h)
  where
    port  = fromMaybe 8000 $ cPort $ hConfig h
    sconf =
        Snap.setPort port $
        Snap.setAccessLog Snap.ConfigNoLog $
        Snap.setErrorLog (Snap.ConfigIoLog (Logger.error (hLogger h))) $
        Snap.defaultConfig

app :: FugaciousM ()
app = handleExceptions $ do
    route <- Routes.parse
    case route of
        GetIndex                       -> do
            mailDomain <- askMailDomain
            Snap.blaze $ Views.index mailDomain
        PostUsers                      -> postUsers
        PostUsersLogout uid            -> authorize uid >>= postUsersLogout
        PostUsersRenew  uid            -> authorize uid >>= postUsersRenew
        GetInbox        uid            -> authorize uid >>= getInbox
        GetInboxMail    uid mid source -> do
            user <- authorize uid
            getInboxMail user mid source
        GetAssetsStyle                -> Snap.serveFile "assets/style.css"

expirationFromNow :: FugaciousM Time.UTCTime
expirationFromNow = do
    lifetime <- askUserLifetime
    now      <- liftIO Time.getCurrentTime
    return $ Time.addUTCTime (fromIntegral lifetime) now

postUsers :: FugaciousM ()
postUsers = do
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
    Snap.redirect $ Routes.renderBs $ GetInbox (Database.uId user)

postUsersLogout :: Database.User -> FugaciousM ()
postUsersLogout user = do
    db <- asks hDatabase
    liftIO $ Database.purgeUser db user
    Snap.redirect $ Routes.renderBs GetIndex

postUsersRenew :: Database.User -> FugaciousM ()
postUsersRenew user = do
    db      <- asks hDatabase
    expires <- expirationFromNow
    liftIO $ Database.renewUser db user expires
    Snap.redirect $ Routes.renderBs $ GetInbox (Database.uId user)

getInbox :: Database.User -> FugaciousM ()
getInbox user = do
    db     <- asks hDatabase
    mails  <- liftIO $ Database.getMailByUser db user
    now    <- liftIO $ Time.getCurrentTime
    Snap.blaze $ Views.inbox now user mails

getInboxMail :: Database.User -> T.Text -> Bool -> FugaciousM ()
getInboxMail user id_ source = do
    db   <- asks hDatabase
    now  <- liftIO $ Time.getCurrentTime
    mail <- liftIO $ Database.getMailById db id_

    if source
        then Snap.plainText $ Database.mSource mail
        else do
            pmail <- case ParseMail.parseMail (Database.mSource mail) of
                Left  err -> Snap.throw500 $ "Could not parser mail: " ++ err
                Right x   -> return x
            Snap.blaze $ Views.mail now user (Database.mId mail) pmail

authorize :: T.Text -> FugaciousM Database.User
authorize id_ = do
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
