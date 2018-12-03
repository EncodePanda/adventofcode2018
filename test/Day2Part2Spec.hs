module Day2Part2Spec where

import Test.Hspec
import Day2

spec :: Spec
spec =
  describe "Day 2, Part 2, diffBy1" $ do
    it "fghij and fguij should diff by 1" $ do
      diffBy1 "fghij" "fguij" `shouldBe` True
    it "abcde and axcye should not diff by 1" $ do
      diffBy1 "abcde" "axcye" `shouldBe` False

