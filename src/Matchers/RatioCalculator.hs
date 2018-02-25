{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Matchers.RatioCalculator where

import Data.Aeson
import GHC.Generics
import Data.List
import Data.Time
import Data.Time.Format
import Data.Time.Clock
import Numeric
import Matchers.Parser
import Utils.Counter

data RatioCalculator = RatioCalculator {
      firstComp :: String
      , secondComp :: String
} deriving (Eq, Generic, Show)

instance FromJSON RatioCalculator
instance Prs RatioCalculator where
    parse a lines = parseWithRatio (firstComp a) (secondComp a) lines

parseWithRatio :: String -> String -> [String] -> String
parseWithRatio m c xs = 
    let (mCount, cCount, ratio) = calculateRatio m c xs
        roundedRatio = showFFloat (Just 2) (ratio * 100) "%"
        roundedRatioFailed = showFFloat (Just 2) ((1.0 - ratio) * 100) "%"
        format f s = f ++ " = " ++ (show s)
    in  ("Successful: " ++ (format m mCount)) ++ "\n" ++ 
        ("Failed: " ++ (format c cCount)) ++ "\n" ++ 
        ("Successful %: " ++ roundedRatio) ++ "\n" ++
        ("Failed %: " ++ roundedRatioFailed) ++ "\n"

calculateRatio :: String -> String -> [String] -> (Float, Float, Float)
calculateRatio m c xs = 
    let mCount = fromIntegral $ countLines m xs 
        cCount = fromIntegral $ countLines c xs
        ratio = if cCount == 0 then 1.0
                else (mCount / (cCount + mCount))
    in (mCount, cCount, ratio)

