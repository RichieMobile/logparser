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

calculateAverageTime :: BeginningComp -> EndComp -> TimeF -> Range -> [FileLines] -> UTCTime
calculateAverageTime bC eC f r lines = do
    let bCMatches = filter (\x -> isMatch bC x) lines
    let bCTimestamps = map (\x -> parseTimestampFromLine f 0 19 x)
    let eCMatches = filter (\x -> isMatch eC x) lines
    let eCTimestamps = map (\x -> parseTimestampFromLine f 0 19 x)
    let timestampTuples = zip bCTimestamps eCTimestamps

calculateAverageTimeWithTuples :: [(UTCTime, UTCTime)] -> UTCTime
calculateAverageTimeWithTuples times = do
    let timeDifferences = map (\(f,s) -> diffUTCTime f s)

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