module Day1Part2Spec where

import Test.Hspec
import Day1

spec :: Spec
spec =
  describe "Day 1, Part 2" $ do
    it "+1, -1 first reaches 0 twice" $ do
      part2 [1, -1] `shouldBe` 0
    it "+3, +3, +4, -2, -4 first reaches 10 twice" $ do
      part2 [3, 3, 4, -2, -4] `shouldBe` 10
    it "-6, +3, +8, +5, -6 first reaches 5 twice" $ do
      part2 [-6, 3, 8, 5, -6] `shouldBe` 5

