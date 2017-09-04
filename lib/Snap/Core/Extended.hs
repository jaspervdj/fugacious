{-# LANGUAGE OverloadedStrings #-}
module Snap.Core.Extended
    ( module Snap.Core

    , FromParam (..)
    , requireParam

    , throw400
    ) where

import qualified Data.ByteString    as B
import           Data.Int           (Int64)
import           Data.Monoid        ((<>))
import qualified Data.Text          as T
import qualified Data.Text.Encoding as T
import           Snap.Core
import           Text.Read          (readMaybe)

class FromParam a where
    fromParam :: B.ByteString -> Maybe a

instance FromParam T.Text where
    fromParam = Just . T.decodeUtf8

instance FromParam Int64 where
    fromParam = readMaybe . T.unpack . T.decodeUtf8

requireParam :: (FromParam a, MonadSnap m) => B.ByteString -> m a
requireParam key = do
    mbBs <- getParam key
    case mbBs of
        Nothing -> throw400 $ "Missing required parameter: " ++ show key
        Just bs -> case fromParam bs of
            Nothing -> throw400 $ "Error decoding parameter: " ++ show key
            Just x  -> return x

throw400 :: MonadSnap m => String -> m a
throw400 reason = do
    modifyResponse $ setResponseStatus 400 "User Error"
    writeBS $ "400 error: " <> T.encodeUtf8 (T.pack reason)
    r <- getResponse
    finishWith r
