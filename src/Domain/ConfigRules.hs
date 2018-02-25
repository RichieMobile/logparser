{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Domain.ConfigRules where

import Data.List
import Data.Aeson
import GHC.Generics
import qualified Domain.Rule as R
import qualified Data.ByteString.Lazy as B

data ConfigRules = ConfigRules {
    rules :: [R.Rule]
} deriving (Eq, Generic, Show)
instance FromJSON ConfigRules

emptyConfig :: ConfigRules
emptyConfig = ConfigRules []

decodeConfig :: B.ByteString -> ConfigRules
decodeConfig config = case (decode config) of
                        Nothing -> emptyConfig
                        Just value -> value
