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