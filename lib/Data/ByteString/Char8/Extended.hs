module Data.ByteString.Char8.Extended
    ( module Data.ByteString.Char8
    , show
    ) where

import           Data.ByteString.Char8
import           Prelude               hiding (show)
import qualified Prelude

show :: Show a => a -> ByteString
show = pack . Prelude.show
