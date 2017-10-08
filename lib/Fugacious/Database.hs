{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
module Fugacious.Database
    ( Error (..)

    , Config (..)

    , Handle (..)
    , withHandle

    , User (..)
    , createUser
    , getUserById
    , getExpiredUsers
    , purgeUser

    , Mail (..)
    , deliverMail
    , getMailById
    , getMailByUser
    ) where

import           Control.Exception                  (Exception, catch, throwIO)
import           Control.Monad                      (replicateM, void)
import qualified Data.Aeson                         as Aeson
import           Data.Maybe                         (fromMaybe)
import           Data.Monoid                        (Last (..))
import qualified Data.Pool                          as Pool
import qualified Data.Text                          as T
import qualified Data.Text.Encoding                 as T
import qualified Data.Time                          as Time
import qualified Data.UUID                          as UUID
import qualified Data.UUID.V4                       as UUID
import qualified Database.PostgreSQL.Simple         as Postgres
import qualified Database.PostgreSQL.Simple.FromRow as Postgres
import           System.Random                      (randomRIO)

data Error
    = Constraint String
    | NotFound String

instance Show Error where
    show (Constraint msg) = msg
    show (NotFound   msg) = msg

instance Exception Error

data Config = Config
    { cConnectionString :: Last T.Text
    } deriving (Show)

instance Monoid Config where
    mempty                        = Config mempty
    mappend (Config l) (Config r) = Config (l `mappend` r)

instance Aeson.FromJSON Config where
    parseJSON = Aeson.withObject "FromJSON Config" $ \o ->
        Config <$> o Aeson..: "connection_string"

data Handle = Handle
    { hConfig :: Config
    , hPool   :: Pool.Pool Postgres.Connection
    }

withHandle :: Config -> (Handle -> IO a) -> IO a
withHandle config f = do
    pool <- Pool.createPool
        (Postgres.connectPostgreSQL $ T.encodeUtf8 connString)
        Postgres.close
        1   -- Number of sub-pools
        30  -- Seconds to keep a resource open
        4   -- Number of resources per sub-pool

    let handle = Handle config pool
    createTables handle

    x <- f handle

    Pool.destroyAllResources pool
    return x
  where
    connString = fromMaybe "" $ getLast $ cConnectionString config

createTables :: Handle -> IO ()
createTables h = do
    Pool.withResource (hPool h) $ \conn -> do
        void $ Postgres.execute_ conn
            "CREATE TABLE IF NOT EXISTS users ( \
            \    id TEXT PRIMARY KEY NOT NULL, \
            \    address TEXT NOT NULL, \
            \    token TEXT NOT NULL, \
            \    expires TIMESTAMPTZ NOT NULL \
            \)"
        void $ Postgres.execute_ conn
            "CREATE UNIQUE INDEX IF NOT EXISTS users_address ON users(address)"
        void $ Postgres.execute_ conn
            "CREATE TABLE IF NOT EXISTS mails ( \
            \    id TEXT PRIMARY KEY NOT NULL, \
            \    \"from\" TEXT NOT NULL, \
            \    \"to\" TEXT NOT NULL, \
            \    subject TEXT NOT NULL, \
            \    source TEXT NOT NULL \
            \)"
        void $ Postgres.execute_ conn
            "CREATE UNIQUE INDEX IF NOT EXISTS mails_to ON mails(id, \"to\")"

data User = User
    { uId      :: T.Text
    , uAddress :: T.Text
    , uToken   :: T.Text
    , uExpires :: Time.UTCTime
    } deriving (Show)

instance Postgres.FromRow User where
    fromRow = User
        <$> Postgres.field
        <*> Postgres.field
        <*> Postgres.field
        <*> Postgres.field

genToken :: IO T.Text
genToken = do
    let chars = ['a' .. 'z'] ++ ['0' .. '9']
        len   = length chars
    fmap T.pack $ replicateM 32 $ do
        idx <- randomRIO (0, len - 1)
        return $ chars !! idx

createUser :: Handle -> T.Text -> Time.UTCTime -> IO User
createUser h address expires = do
    uuid  <- UUID.toText <$> UUID.nextRandom
    token <- genToken
    Pool.withResource (hPool h) $ \conn ->
        catch (do
            void $ Postgres.execute conn
                "INSERT INTO users (id, address, token, expires) \
                \VALUES (?, ?, ?, ?)"
                (uuid, address, token, expires)
            return $ User uuid address token expires)
            (\err -> case err of
                Postgres.SqlError {..} -> throwIO $ Constraint $
                    "The address " ++ T.unpack address ++ " is already taken.")

getUserById :: Handle -> T.Text -> IO User
getUserById h uuid = Pool.withResource (hPool h) $ \conn -> do
    users <- Postgres.query conn
        "SELECT id, address, token, expires FROM users WHERE id = ?"
        (Postgres.Only uuid)
    case users of
        (user : _) -> return user
        []         -> throwIO $ NotFound $
            "User " ++ T.unpack uuid ++ " does not exist."

getUserByAddress :: Handle -> T.Text -> IO User
getUserByAddress h address = Pool.withResource (hPool h) $ \conn -> do
    users <- Postgres.query conn
        "SELECT id, address, token, expires FROM users WHERE address = ?"
        (Postgres.Only address)
    case users of
        (user : _) -> return user
        []         -> throwIO $ NotFound $
            "User " ++ T.unpack address ++ " does not exist."

getExpiredUsers :: Handle -> IO [User]
getExpiredUsers h = Pool.withResource (hPool h) $ \conn -> do
    now <- Time.getCurrentTime
    Postgres.query conn
        "SELECT id, address, token, expires FROM users WHERE expires < ?"
        (Postgres.Only now)

purgeUser :: Handle -> User -> IO ()
purgeUser h user = Pool.withResource (hPool h) $ \conn -> do
    void $ Postgres.execute conn
        "DELETE FROM users WHERE id = ?" (Postgres.Only (uId user))
    void $ Postgres.execute conn
        "DELETE FROM mails WHERE \"to\" = ?" (Postgres.Only (uAddress user))

data Mail = Mail
    { mId      :: T.Text
    , mFrom    :: T.Text
    , mTo      :: T.Text
    , mSubject :: T.Text
    , mSource  :: T.Text
    } deriving (Show)

instance Postgres.FromRow Mail where
    fromRow = Mail
        <$> Postgres.field
        <*> Postgres.field
        <*> Postgres.field
        <*> Postgres.field
        <*> Postgres.field

deliverMail
    :: Handle
    -> T.Text  -- ^ From
    -> T.Text  -- ^ To
    -> T.Text  -- ^ Subject
    -> T.Text  -- ^ Full source
    -> IO Mail
deliverMail h mFrom mTo mSubject mSource = do
    _user <- getUserByAddress h mTo
    mId   <- UUID.toText <$> UUID.nextRandom
    Pool.withResource (hPool h) $ \conn -> do
        void $ Postgres.execute conn
            "INSERT INTO mails (id, \"from\", \"to\", subject, source) \
            \VALUES (?, ?, ?, ?, ?)"
            (mId, mFrom, mTo, mSubject, mSource)
        return Mail {..}

getMailById :: Handle -> T.Text -> IO Mail
getMailById h mId = Pool.withResource (hPool h) $ \conn -> do
    mails <- Postgres.query conn
        "SELECT id, \"from\", \"to\", subject, source \
        \FROM mails WHERE id = ?" (Postgres.Only mId)
    case mails of
        (mail : _) -> return mail
        []          -> throwIO $ NotFound $
            "Email " ++ T.unpack mId ++ " does not exist."

getMailByUser :: Handle -> User -> IO [Mail]
getMailByUser h user = Pool.withResource (hPool h) $ \conn -> do
    Postgres.query conn
        "SELECT id, \"from\", \"to\", subject, source \
        \FROM mails WHERE \"to\" = ?" (Postgres.Only (uAddress user))
