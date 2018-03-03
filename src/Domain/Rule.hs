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
import Control.Monad

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
  parse a filePath = do
    output <- parseWithParsers a filePath
    return output

parseWithParsers :: Rule -> [FilePath] -> IO String
parseWithParsers rule lines = do
  ct <- foldM (\x c -> fmap (x++) (parse c lines)) "" (counters rule)
  rt <- foldM (\x r -> fmap (x++) (parse r lines)) "" (ratios rule)
  ta <- foldM (\x t -> fmap (x++) (parse t lines)) "" (timeAvgs rule)
  let output = "\nCount Match\n" ++ ct ++ 
               "\n\nRatio Match\n" ++ rt ++ 
               "\nAverage Time to Complete\n" ++ ta

  return output