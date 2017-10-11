{-# LANGUAGE OverloadedStrings #-}
module Fugacious.User
    ( Config (..)
    ) where

import           Control.Applicative (empty, (<|>))
import qualified Data.Aeson          as A
import qualified Data.Text           as T

data Config = Config
    { cDomain   :: Maybe T.Text
    , cLifetime :: Maybe Int
    }

instance Monoid Config where
    mempty        = Config empty empty
    mappend c1 c2 = Config
        { cDomain   = cDomain c1   <|> cDomain c2
        , cLifetime = cLifetime c1 <|> cLifetime c2
        }

instance A.FromJSON Config where
    parseJSON = A.withObject "FromJSON Fugacious.User.Config" $ \o -> Config
        <$> o A..:? "domain"
        <*> o A..:? "lifetime"
