module LogParserSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck
import Matchers.Counter
import Matchers.RatioCalculator
import Matchers.TimeAverager
import Utils.Counter

import LogParser
-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "parse" $ do
    it "Should read a log file and a config file and display processor output properly" $ do
      let expected ="\nCount Match\n\
        \conreq = 2\n\n\
        \Ratio Match\n\
        \Successful: conreq = 2.0\n\
        \Failed: Rest Request = 2.0\n\
        \Successful %: 50.00%\n\
        \Failed %: 50.00%\n\n\
        \Average Time to Complete\n\
        \Comparitor 1: conreq\
        \Comparitor 2: Rest Request\
        \Average time to complete (minutes): 33\n"

      parse "test/testfiles/config.json" ["test/testfiles/log.log"] `shouldReturn` expected

  describe "parseWithMatcher" $ do
    it "Should return formated string output `matcher` = `count`" $
      parseWithMatcher "conreq" ["conreq", "conreq"] `shouldBe` "conreq = 2"

  describe "countLines" $ do
    it "Should take a list of lines from a log and sum the total matching parameters" $
      countLines "conreq" ["conreq", "conreq"] `shouldBe` 2

    it "Should return 0 when passed an empty list of lines" $
      countLines "conreq" [] `shouldBe` 0

    it "Should return 0 when passed an empty string as a matcher" $
      countLines "" ["conreq"] `shouldBe` 0

  describe "count" $ do
    it "Should take a line from a log and count the number of matching parameters" $
      count "conreq" "conreq completed succesfully" `shouldBe` 1

  describe "calculateRatio" $ do
    it "Should use matcher and comparator to create a ratio from the log file" $
      calculateRatio "conreq" "fun" ["conreq", "conreq", "fun", "fun", "fun"] `shouldBe` (2.0, 3.0, 0.4) 

  describe "parseWithRatio" $ do
    it "Should use matcher and comparator to create a ratio from the log file" $ do
      let expected = "Successful: conreq = 2.0\nFailed: fun = 3.0\n\
        \Successful %: 40.00%\nFailed %: 60.00%\n"
      let lines = ["conreq", "conreq", "fun", "fun", "fun"]
      parseWithRatio "conreq" "fun" lines `shouldBe` expected