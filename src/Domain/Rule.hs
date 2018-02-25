{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Domain.Rule where

import Data.List
import Data.Aeson
import GHC.Generics
import Matchers.TimeAverager
import Matchers.Counter
import Matchers.RatioCalculator
import Matchers.Parser

type Matcher = String

counter = "count"
ratio = "ratio"
avgTime = "avgTime"

emptyRule :: Rule
emptyRule = Rule [] [] []

data Rule = Rule {
    counters :: [Counter]
  , ratios :: [RatioCalculator]
  , timeAvgs :: [AverageTimeParser]
} deriving (Eq, Generic, Show)
instance FromJSON Rule

instance Prs Rule where
  parse a line = parseWithParsers a line

parseWithParsers :: Rule -> [String] -> String
parseWithParsers rule lines = do
  let ct = foldl (\x c -> x ++ (parse c lines)) "" (counters rule)
  let rt = foldl (\x r -> x ++ (parse r lines)) "" (ratios rule)
  let ta = foldl (\x t -> x ++ (parse t lines)) "" (timeAvgs rule)
  ct ++ "\n" ++ rt ++ "\n" ++ ta