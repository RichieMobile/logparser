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
  describe "LogParser" $ do
    it "Should read a log file and a config file and return the number of matching lines that are found" $ do
      parse "test/testfiles/log.log" "test/testfiles/config.cfg" `shouldReturn` "conreq = 2"

    it "Should take a line from a log and count the number of matching parameters" $
      count "conreq" "conreq completed succesfully" `shouldBe` 1

    it "Should take a list lines from a log and sum the total matching patameters" $
      countLines "conreq" ["conreq", "conreq"] `shouldBe` 2

    it "Should return 0 when passed an empty list of lines" $
      countLines "conreq" [] `shouldBe` 0

    it "Should return 0 when passed an empty string as a matcher" $
      countLines "" ["conreq"] `shouldBe` 0

    it "Should return formated string output `matcher` = `count`" $
      parseWithMatcher "conreq" ["conreq", "conreq"] `shouldBe` "conreq = 2"