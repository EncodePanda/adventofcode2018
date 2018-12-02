module Da2Part1Spec where

import Test.Hspec
import Day2

spec :: Spec
spec =
  describe "Day 2, Part 1" $ do
    it "abcde control is 0" $ do
      part1 ["abcde"] `shouldBe` 0
    it "aabbb control is 1" $ do
      part1 ["aabbb"] `shouldBe` 1
    it "[abcdef, bababc, abbcde, abcccd, aabcdd, abcdee, ababab] control is 12" $ do
      part1 ["abcdef", "bababc", "abbcde", "abcccd", "aabcdd", "abcdee", "ababab"] `shouldBe` 12


