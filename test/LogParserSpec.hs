module LogParserSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import LogParser
-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "parse" $ do
    it "Should read a log file and a config file and display processor output properly" $ do
      let expected = "conreq = 2\nconreq = 2.0\nRest Request = 2.0\nconreq/Rest Request = \"0.50\"\n"
      parse "test/testfiles/log.log" "test/testfiles/config.json" `shouldReturn` expected

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
      let expected = "conreq = 2.0\nfun = 3.0\nconreq/fun = \"0.40\""
      let lines = ["conreq", "conreq", "fun", "fun", "fun"]
      parseWithRatio "conreq" "fun" lines `shouldBe` expected