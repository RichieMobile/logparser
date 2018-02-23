{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Domain.Rule where

import Data.List
import Data.Aeson
import GHC.Generics

type Matcher = String

counter = "count"
ratio = "ratio"
avgTime = "avgTime"

data Rule = Rule {
      matcher :: Matcher
    , comparitor :: String
    , processor :: String
    , timestampFormat :: String
    , range :: String
} deriving (Eq, Generic, Show)

instance FromJSON Rule