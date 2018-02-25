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

-- data Rule = Rule {
--       matcher :: Matcher
--     , comparitor :: String
--     , processor :: String
--     , timestampFormat :: String
--     , range :: String
-- } deriving (Eq, Generic, Show)
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
  let ct = "Count Match\n" ++ (foldl (\x c -> x ++ (parse c lines)) "" (counters rule)) ++ "\n"
  let rt = "Ratio Match\n" ++ (foldl (\x r -> x ++ (parse r lines)) "" (ratios rule)) ++ "\n"
  let ta = "Average Time to Complete\n" ++ (foldl (\x t -> x ++ (parse t lines)) "" (timeAvgs rule))
  "\n" ++ ct ++ "\n" ++ rt ++ ta