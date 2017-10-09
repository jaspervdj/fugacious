-- | Supporting "Content-Transfer-Encoding: quoted-printable"
{-# LANGUAGE OverloadedStrings #-}
module Network.QuotedPrintable
    ( decode
    ) where

import           Data.Char      (chr)
import           Data.Monoid    ((<>))
import qualified Data.Text.Lazy as TL
import           Numeric        (readHex)

decode :: TL.Text -> TL.Text
decode t0
    -- End of encoding
    | TL.null t1 || TL.null t2 = pre
    -- Soft linebreak
    | "\r\n" `TL.isPrefixOf` t2 = pre <> decode (TL.drop 2 t2)
    | "\n" `TL.isPrefixOf` t2 = pre <> decode (TL.drop 1 t2)
    -- Hex-encoded character
    | [(n, "")]  <- readHex (TL.unpack $ TL.take 2 t2) =
        pre <> TL.singleton (chr n) <> decode (TL.drop 2 t2)
    | otherwise =
        pre <> "=" <> decode t2
  where
    (pre, t1) = TL.break (== '=') t0
    t2        = TL.drop 1 t1
