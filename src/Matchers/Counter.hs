{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Matchers.Counter where

import Data.List
import Data.Aeson
import GHC.Generics
import Matchers.Parser
import Utils.Counter

data Counter = Counter {
      matcher :: String
} deriving (Eq, Generic, Show)

instance FromJSON Counter
instance Prs Counter where
    parse a lines = parseWithMatcher (matcher a) lines

parseWithMatcher :: String -> [String] -> String
parseWithMatcher m xs = m ++ " = " ++ (show $ countLines m xs)