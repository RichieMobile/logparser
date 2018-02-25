module Matchers.Parser where

-- import Matchers.TimeAverager
-- import Matchers.Counter
-- import Matchers.RatioCalculator

class Prs a where
    parse :: a -> [String] -> String

-- parse :: a -> [String] -> String
-- parse (Counter m) lines = parseWithMatcher m lines
-- parse (RatioCalculator f s) lines = parseWithRatio f s lines
-- parse (AverageTimeParser b e f r) lines = parseAverageTime b e f r lines
-- parse _ lines = "Unknown Parser"