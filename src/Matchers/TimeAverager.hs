module Matchers.TimeAverager where

import Data.Time
import Data.Time.Format
import Data.Time.Clock
import qualified LogParser as L

type BeginningComp = String
type EndComp = String
type TimeF = String
type FileLines = String
type Range = String

calculateAverageTime :: BeginningComp -> EndComp -> TimeF -> Range -> [FileLines] -> Integer
calculateAverageTime bC eC f r lines = do
    let bCMatches = filter (\x -> isMatch bC x) lines
    let bCTimestamps = map (\x -> parseTimestampFromLine f 3 22 x) bCMatches
    let eCMatches = filter (\x -> isMatch eC x) lines
    let eCTimestamps = map (\x -> parseTimestampFromLine f 3 22 x) eCMatches
    let timestampTuples = zip bCTimestamps eCTimestamps
    calculateAverageTimeWithTuples timestampTuples

calculateAverageTimeWithTuples :: [(UTCTime, UTCTime)] -> Integer
calculateAverageTimeWithTuples times = do
    let timeDifferences = map (\x -> diffTimeToSeconds x) times
    let sumDiff = sum timeDifferences :: Integer
    let lengthDiff = length timeDifferences
    (sumDiff `div` (toInteger lengthDiff)) `div` 60
    -- sumDiff `div` 10

diffTimeToSeconds :: (UTCTime, UTCTime) -> Integer
diffTimeToSeconds (f,s) = do
    let (diffTime, _) = properFraction $ diffUTCTime f s
    toInteger diffTime

isMatch :: String -> String -> Bool
isMatch matcher line = 
    case L.count matcher line of
        1 -> True
        _ -> False

parseTimestampFromLine :: TimeF -> Int -> Int -> String -> UTCTime
parseTimestampFromLine format begining end line = do
    let timestamp = drop begining (take end line)
    let utcTimestamp = parseTimestamp format timestamp
    utcTimestamp

parseTimestamp :: String -> String -> UTCTime
parseTimestamp f t = parseTimeOrError True defaultTimeLocale f t 