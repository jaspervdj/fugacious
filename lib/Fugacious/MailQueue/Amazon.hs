-- | This module provides incoming mail through Amazon Simple Email Service.
-- The setup is a bit complex but works well.
--
-- * SES should be configured to publish incoming mail on an SNS topic.
-- * We need to have an SQS queue which is subscribed to this topic.
-- * This module simply needs to pull items from the queue and delete them.
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Fugacious.MailQueue.Amazon
    ( Config (..)
    , withHandle
    ) where

import           Control.Lens                 ((&), (.~), (^.))
import           Control.Monad                (forM_)
import           Control.Monad.Trans          (liftIO)
import qualified Data.Aeson                   as A
import qualified Data.Binary.Builder          as Builder
import qualified Data.Text                    as T
import qualified Data.Text.Lazy               as TL
import qualified Data.Text.Lazy.Encoding      as TL
import qualified Fugacious.Logger             as Logger
import           Fugacious.MailQueue.Internal
import qualified Network.AWS                  as Aws
import qualified Network.AWS.SQS              as Aws.Sqs

data Config = Config
    { cQueueUrl :: !T.Text
    }

instance A.FromJSON Config where
    parseJSON = A.withObject "FromJSON Fugacious.MailQueue.Amazon.Config" $ \o ->
        Config <$> o A..: "queue_url"

withHandle :: Config -> Logger.Handle -> (Handle -> IO a) -> IO a
withHandle config logger f = do
    env <- Aws.newEnv Aws.Discover
    f Handle
        { pop = popWith config logger env
        }

popWith :: Config -> Logger.Handle -> Aws.Env -> DeliverMail -> IO ()
popWith config logger env deliver = do
    Aws.runResourceT $ Aws.runAWS (env & Aws.envLogger .~ awsLogger) $ do
        response <- Aws.send $ Aws.Sqs.receiveMessage queueUrl
        forM_ (response ^. Aws.Sqs.rmrsMessages) $ \message -> do
            -- Parse and deliver.
            liftIO $ case parseMessage message of
                Left err  -> Logger.error logger err
                Right txt -> deliver txt

            -- After receiving, delete the message using the receipt handle.
            case message ^. Aws.Sqs.mReceiptHandle of
                Nothing      -> return ()
                Just receipt -> do
                    _ <- Aws.send $ Aws.Sqs.deleteMessage queueUrl receipt
                    return ()
  where
    queueUrl = cQueueUrl config
    awsLogger level builder = case level of
        Aws.Error -> Logger.error logger (Builder.toLazyByteString builder)
        _         -> return ()

-- | This is a complete shitshow, since Amazon likes to encode JSON as strings
-- and then put this inside other JSON objects.  That requires us to peel off
-- the layers one by one before we can get to the "actual content".
parseMessage :: Aws.Sqs.Message -> Either String T.Text
parseMessage msg = do
    -- Parse body
    body <- case msg ^. Aws.Sqs.mBody of
        Nothing -> Left "Message does not have a body"
        Just x  -> Right x

    -- Inspect JSON
    Notification n <- case A.decode (TL.encodeUtf8 $ TL.fromStrict body) of
        Nothing -> Left "Could not parse notification JSON"
        Just x  -> Right x

    -- Again...
    Message x <- case A.decode (TL.encodeUtf8 $ TL.fromStrict n) of
        Nothing -> Left "Could not parse message JSON"
        Just x  -> Right x

    return x

newtype Notification = Notification T.Text

instance A.FromJSON Notification where
    parseJSON = A.withObject "FromJSON Notification" $ \o ->
        Notification <$> o A..: "Message"

newtype Message = Message T.Text

instance A.FromJSON Message where
    parseJSON = A.withObject "FromJSON Message" $ \o ->
        Message <$> o A..: "content"
