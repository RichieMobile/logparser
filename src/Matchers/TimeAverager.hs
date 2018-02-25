{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Matchers.TimeAverager where

import Data.Aeson
import GHC.Generics
import Data.List
import Data.Time
import Data.Time.Clock
import Matchers.Parser
import Utils.Counter

type BeginningComp = String
type EndComp = String
type TimeF = String
type FileLines = String
type Range = String

data AverageTimeParser = AverageTimeParser {
      beginningComp :: String
    , endComp :: String
    , timeFormat :: String
    , range :: String
} deriving (Eq, Generic, Show)

instance FromJSON AverageTimeParser
instance Prs AverageTimeParser where
    parse a lines = parseAverageTime (beginningComp a) (endComp a) (timeFormat a) (range a) lines

parseAverageTime :: BeginningComp -> EndComp -> TimeF -> Range -> [FileLines] -> String
parseAverageTime bC eC f r lines =
    "Comparitor 1: " ++ bC ++
    "Comparitor 2: " ++ eC ++
    "Average time to complete (minutes): " ++ show (calculateAverageTime bC eC f r lines)

calculateAverageTime :: BeginningComp -> EndComp -> TimeF -> Range -> [FileLines] -> Integer
calculateAverageTime bC eC f r lines = 
    case parseRange r of
        Nothing -> -1
        Just a -> calculateAverageTime' bC eC f a lines

calculateAverageTime' :: BeginningComp -> EndComp -> TimeF -> (Int, Int) -> [FileLines] -> Integer
calculateAverageTime' bC eC f (end, beg) lines = do
    let bCMatches = filter (\x -> isMatch bC x) lines
    let bCTimestamps = map (\x -> parseTimestampFromLine f end beg x) bCMatches
    let eCMatches = filter (\x -> isMatch eC x) lines
    let eCTimestamps = map (\x -> parseTimestampFromLine f end beg x) eCMatches
    let timestampTuples = zip bCTimestamps eCTimestamps
    calculateAverageTimeWithTuples timestampTuples

parseRange :: Range -> Maybe (Int, Int)
parseRange [] = Nothing
parseRange (x:xs) | x == '-' = Nothing
parseRange range = 
    case elemIndex '-' range of 
        Just i -> if i == (length range) - 1
                    then Nothing 
                    else parseRange' range i
        Nothing -> Nothing

parseRange' :: Range -> Int -> Maybe (Int, Int)
parseRange' r i = do 
    let beginning =  read (take i r) :: Int
    let end = read (drop (i + 1) r) :: Int
    Just (beginning, end)

isMatch :: String -> String -> Bool
isMatch matcher line = 
    case count matcher line of
        1 -> True
        _ -> False

parseTimestampFromLine :: TimeF -> Int -> Int -> String -> UTCTime
parseTimestampFromLine format begining end line = do
    let timestamp = drop begining (take end line)
    let utcTimestamp = parseTimestamp format timestamp
    utcTimestamp

parseTimestamp :: String -> String -> UTCTime
parseTimestamp f t = parseTimeOrError True defaultTimeLocale f t 

calculateAverageTimeWithTuples :: [(UTCTime, UTCTime)] -> Integer
calculateAverageTimeWithTuples times = do
    let timeDifferences = map (\x -> diffTimeToSeconds x) times
    let sumDiff = sum timeDifferences :: Integer
    let lengthDiff = length timeDifferences
    (sumDiff `div` (toInteger lengthDiff)) `div` 60

diffTimeToSeconds :: (UTCTime, UTCTime) -> Integer
diffTimeToSeconds (f,s) = do
    let (diffTime, _) = properFraction $ diffUTCTime s f
    toInteger diffTime

