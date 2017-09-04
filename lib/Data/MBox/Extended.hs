{-# LANGUAGE OverloadedStrings #-}
module Data.MBox.Extended
    ( module Data.MBox
    , makeMessage
    ) where

import           Data.MBox
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Time as Time
import Data.Monoid ((<>))

makeMessage
    :: T.Text  -- ^ From
    -> T.Text  -- ^ To
    -> T.Text  -- ^ Subject
    -> T.Text  -- ^ Body
    -> IO Message
makeMessage from to subject body_ = do
    utc <- Time.getCurrentTime
    return Message
        { fromLine = "From " <> TL.fromStrict from <> " " <> TL.pack (asctime utc)
        , body     = TL.fromStrict body_
        , headers  =
            [ ("From", TL.fromStrict from)
            , ("To", TL.fromStrict to)
            , ("Subject", TL.fromStrict subject)
            ]
        }
  where
    asctime = Time.formatTime Time.defaultTimeLocale "%a %b %d %H:%M:%S %Y"
