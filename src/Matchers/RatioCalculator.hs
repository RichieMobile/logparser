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
-- instance Prs RatioCalculator where
--     parse a lines = parseWithRatio (firstComp a) (secondComp a) lines
instance Prs RatioCalculator where
    parse avgP paths = do
        matches <- mapM (\files -> countMatches' (firstComp avgP) (secondComp avgP) files) paths
        let (x, y) = foldl (\a b -> addCounts a b) (0, 0) matches
        let ratio = x / (x + y)
        return $ presentRatio (firstComp avgP) (secondComp avgP) (x, y, ratio)

countMatches' :: String -> String -> FilePath -> IO (Float, Float)
countMatches' m c p = do
    contents <- readFile p
    let l = lines contents
    let tup = countMatches m c l
    return tup

countMatches :: String -> String -> [String] -> (Float, Float)
countMatches m c xs = 
    let mCount = fromIntegral $ countLines m xs 
        cCount = fromIntegral $ countLines c xs
    in (mCount, cCount)

addCounts :: (Float, Float) -> (Float, Float) -> (Float, Float)
addCounts (a,b) (c,d) = (a + c, b + d) 

presentRatio :: String -> String -> (Float, Float, Float) -> String
presentRatio c1 c2 (a, b, r) = 
    let roundedRatio = showFFloat (Just 2) (r * 100) "%"
        roundedRatioFailed = showFFloat (Just 2) ((1.0 - r) * 100) "%"
        format f s = f ++ " = " ++ (show s)
    in  ("Successful: " ++ (format c1 a)) ++ "\n" ++ 
        ("Failed: " ++ (format c2 b)) ++ "\n" ++ 
        ("Successful %: " ++ roundedRatio) ++ "\n" ++
        ("Failed %: " ++ roundedRatioFailed) ++ "\n"

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
    let (mCount, cCount) = countMatches m c xs
        ratio = if cCount == 0 then 1.0
                else (mCount / (cCount + mCount))
    in (mCount, cCount, ratio)



