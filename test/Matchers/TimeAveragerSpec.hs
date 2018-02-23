module Matchers.TimeAveragerSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import Matchers.TimeAverager
-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "TimeAverager" $ do
    it "Should be able to parse timestamp from beginning of log" $ do
        let dateLine = "2018-02-20 09:06:54 INFO: Yes Yes Y'all"
        let format = "%Y-%m-%d %T"
        show (parseTimestampFromLine format 0 19 dateLine) `shouldBe` "2018-02-20 09:06:54 UTC"

    it "Should be able to parse timestamp from middle of log" $ do
        let dateLine = "INFO: 2018-02-20 09:06:54 - Yes Yes Y'all"
        let format = "%Y-%m-%d %T"
        show (parseTimestampFromLine format 6 25 dateLine) `shouldBe` "2018-02-20 09:06:54 UTC"

    it "Should be able to parse timestamp with format" $ do
        let dateLine = "2018-02-20 09:06:54"
        let format = "%Y-%m-%d %T"
        show (parseTimestamp format dateLine) `shouldBe` "2018-02-20 09:06:54 UTC"

    it "Should calculate average time in picoseconds" $ do
        let dateLine = ["C: 2018-02-20 09:06:54", "D: 2018-02-20 10:06:54"]  ++
                       ["C: 2018-02-20 10:06:54", "D: 2018-02-20 10:12:54"]
        let format = "%Y-%m-%d %T"
        show (calculateAverageTime "C:" "D:" format "3-22" dateLine) `shouldBe` "33"

    it "It should parse range properly" $ do
        let range = "2-13"
        parseRange range `shouldBe` Just (2, 13)

    it "Should return Nothing when range is empty" $ do
        let range = ""
        parseRange range `shouldBe` Nothing

    it "Should return Nothing when range starts with -" $ do
        let range = "-15"
        parseRange range `shouldBe` Nothing

    it "Should return Nothing when range ends with -" $ do
        let range = "14-"
        parseRange range `shouldBe` Nothing




    