module Matchers.TimeAverager where

import Data.Time
import Data.Time.Format

type BeginningComp = String
type EndComp = String
type TimeF = String

calculateAverageTime :: BeginningComp -> EndComp -> TimeF -> String -> String -> UTCTime

parseTimestampFromLine :: TimeF -> Int -> Int -> String -> UTCTime
parseTimestampFromLine format begining end line = do
    let timestamp = take (end - begining) (take end line)
    let utcTimestamp = parseTimestamp format timestamp
    utcTimestamp

parseTimestamp :: String -> String -> UTCTime
parseTimestamp f t = parseTimeOrError True defaultTimeLocale f t 