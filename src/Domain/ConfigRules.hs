{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Domain.ConfigRules where

import Data.List
import Data.Aeson
import GHC.Generics
import Domain.Rule
import qualified Data.ByteString.Lazy as B

data ConfigRules = ConfigRules {
    rules :: [Rule]
} deriving (Eq, Generic, Show)

emptyConfig :: ConfigRules
emptyConfig = ConfigRules [Rule ""]

instance FromJSON ConfigRules

decodeConfig :: B.ByteString -> ConfigRules
decodeConfig config = case (decode config) of
                        Nothing -> emptyConfig
                        Just value -> value
