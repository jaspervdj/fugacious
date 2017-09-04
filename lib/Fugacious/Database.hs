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

import           Control.Exception       (Exception, catch, throwIO)
import           Control.Monad           (replicateM)
import qualified Data.Aeson              as Aeson
import           Data.Maybe              (fromMaybe)
import           Data.Monoid             ((<>))
import           Data.Monoid             (Last (..))
import qualified Data.Pool               as Pool
import qualified Data.Text               as T
import qualified Data.Time               as Time
import qualified Data.UUID               as UUID
import qualified Data.UUID.V4            as UUID
import qualified Database.SQLite.Simple  as Sqlite
import           System.Random           (randomRIO)

data Error
    = Constraint T.Text
    | NotFound T.Text

instance Show Error where
    show (Constraint msg) = T.unpack msg
    show (NotFound   msg) = T.unpack msg

instance Exception Error

data Config = Config
    { cPath :: Last FilePath
    } deriving (Show)

instance Monoid Config where
    mempty                        = Config mempty
    mappend (Config l) (Config r) = Config (l `mappend` r)

instance Aeson.FromJSON Config where
    parseJSON = Aeson.withObject "FromJSON Config" $ \o ->
        Config <$> o Aeson..: "path"

data Handle = Handle
    { hConfig :: Config
    , hPool   :: Pool.Pool Sqlite.Connection
    }

withHandle :: Config -> (Handle -> IO a) -> IO a
withHandle config f = do
    pool <- Pool.createPool
        (Sqlite.open path)
        Sqlite.close
        1   -- Number of sub-pools
        30  -- Seconds to keep a resource open
        4   -- Number of resources per sub-pool

    let handle = Handle config pool
    createTables handle

    x <- f handle

    Pool.destroyAllResources pool
    return x
  where
    path = fromMaybe "fugacious.db" $ getLast $ cPath config

createTables :: Handle -> IO ()
createTables h = do
    Pool.withResource (hPool h) $ \conn -> do
        Sqlite.execute_ conn
            "CREATE TABLE IF NOT EXISTS users ( \
            \    id TEXT PRIMARY KEY NOT NULL, \
            \    address TEXT NOT NULL, \
            \    token TEXT NOT NULL, \
            \    expires TEXT NOT NULL \
            \)"
        Sqlite.execute_ conn
            "CREATE UNIQUE INDEX IF NOT EXISTS users_address ON users(address)"
        Sqlite.execute_ conn
            "CREATE TABLE IF NOT EXISTS mails ( \
            \    id TEXT PRIMARY KEY NOT NULL, \
            \    \"from\" TEXT NOT NULL, \
            \    \"to\" TEXT NOT NULL, \
            \    subject TEXT NOT NULL, \
            \    source TEXT NOT NULL \
            \)"
        Sqlite.execute_ conn
            "CREATE UNIQUE INDEX IF NOT EXISTS mails_to ON mails(id, \"to\")"

data User = User
    { uId      :: T.Text
    , uAddress :: T.Text
    , uToken   :: T.Text
    , uExpires :: Time.UTCTime
    } deriving (Show)

instance Sqlite.FromRow User where
    fromRow =
        User <$> Sqlite.field <*> Sqlite.field <*> Sqlite.field <*> Sqlite.field

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
            Sqlite.execute conn
                "INSERT INTO users (id, address, token, expires) \
                \VALUES (?, ?, ?, ?)"
                (uuid, address, token, expires)
            return $ User uuid address token expires)
            (\err -> case err of
                Sqlite.SQLError {Sqlite.sqlError = Sqlite.ErrorConstraint} ->
                    throwIO $ Constraint $
                        "The address " <> address <> " is already taken."
                _ -> throwIO err)

getUserById :: Handle -> T.Text -> IO User
getUserById h uuid = Pool.withResource (hPool h) $ \conn -> do
    users <- Sqlite.query conn
        "SELECT id, address, token, expires FROM users WHERE id = ?"
        (Sqlite.Only uuid)
    case users of
        (user : _) -> return user
        []         -> throwIO $ NotFound $
            "User " <> uuid <> " does not exist."

getUserByAddress :: Handle -> T.Text -> IO User
getUserByAddress h address = Pool.withResource (hPool h) $ \conn -> do
    users <- Sqlite.query conn
        "SELECT id, address, token, expires FROM users WHERE address = ?"
        (Sqlite.Only address)
    case users of
        (user : _) -> return user
        []         -> throwIO $ NotFound $
            "User " <> address <> " does not exist."

getExpiredUsers :: Handle -> IO [User]
getExpiredUsers h = Pool.withResource (hPool h) $ \conn -> do
    now   <- Time.getCurrentTime
    Sqlite.query conn
        "SELECT id, address, token, expires FROM users WHERE expires < ?"
        (Sqlite.Only now)

purgeUser :: Handle -> User -> IO ()
purgeUser h user = Pool.withResource (hPool h) $ \conn -> do
    Sqlite.execute conn
        "DELETE FROM users WHERE id = ?" (Sqlite.Only (uId user))
    Sqlite.execute conn
        "DELETE FROM mails WHERE \"to\" = ?" (Sqlite.Only (uAddress user))

data Mail = Mail
    { mId      :: T.Text
    , mFrom    :: T.Text
    , mTo      :: T.Text
    , mSubject :: T.Text
    , mSource  :: T.Text
    } deriving (Show)

instance Sqlite.FromRow Mail where
    fromRow = Mail
        <$> Sqlite.field
        <*> Sqlite.field
        <*> Sqlite.field
        <*> Sqlite.field
        <*> Sqlite.field

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
        Sqlite.execute conn
            "INSERT INTO mails (id, \"from\", \"to\", subject, source) \
            \VALUES (?, ?, ?, ?, ?)"
            (mId, mFrom, mTo, mSubject, mSource)
        return Mail {..}

getMailById :: Handle -> T.Text -> IO Mail
getMailById h mId = Pool.withResource (hPool h) $ \conn -> do
    mails <- Sqlite.query conn
        "SELECT id, \"from\", \"to\", subject, source \
        \FROM mails WHERE id = ?" (Sqlite.Only mId)
    case mails of
        (mail : _) -> return mail
        []          -> throwIO $ NotFound $
            "Email " <> mId <> " does not exist."

getMailByUser :: Handle -> User -> IO [Mail]
getMailByUser h user = Pool.withResource (hPool h) $ \conn -> do
    Sqlite.query conn
        "SELECT id, \"from\", \"to\", subject, source \
        \FROM mails WHERE \"to\" = ?" (Sqlite.Only (uAddress user))
