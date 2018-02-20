{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Domain.Rule where

import Data.List
import Data.Aeson
import GHC.Generics

type Matcher = String

data Rule = Rule {
    matcher :: Matcher
} deriving (Eq, Generic, Show)

instance FromJSON Rule