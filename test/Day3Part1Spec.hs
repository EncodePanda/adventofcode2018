module Day3Part1Spec where

import Test.Hspec
import Day3

spec :: Spec
spec = do
  parseSpec
  parseLineSpec
  part1Spec

parseSpec :: Spec
parseSpec =
  describe "Day 3, Part 1, parse" $ do
    it "fsdsd" $ do
      (parseClaimDesc "#1285 @ 429,871: 21x24") `shouldBe` ClaimDesc 1285 429 871 21 24

-- ........
-- ...2222.
-- ...2222.
-- .11XX22.
-- .11XX22.
-- .111133.
-- .111133.
-- ........
example1 :: [String]
example1 = ["#1 @ 1,3: 4x4", "#2 @ 3,1: 4x4", "#3 @ 5,5: 2x2"]

parseLineSpec :: Spec
parseLineSpec =
  describe "Day 3, Part 1, parseLine" $ do
    it "should find point in right claim" $ do
      (parseLine (parseClaimDesc "#1285 @ 429,871: 21x24") $ (Point (429, 871))) `shouldBe` [1285]
    it "should NOT find point in claim" $ do
      (parseLine (parseClaimDesc "#1285 @ 429,871: 21x24") $ (Point (428, 870))) `shouldBe` []
    it "should parse each line of example1 correctly" $ do
      -- ........
      (fmap (parseLines (fmap parseClaimDesc example1)) [Point (x, 0) | x <- [0..7]]) `shouldBe` [[],[],[],[],[],[],[],[]]
      -- ...2222.
      (fmap (parseLines (fmap parseClaimDesc example1)) [Point (x, 1) | x <- [0..7]]) `shouldBe` [[],[],[],[2],[2],[2],[2],[]]
      -- ...2222.
      (fmap (parseLines (fmap parseClaimDesc example1)) [Point (x, 2) | x <- [0..7]]) `shouldBe` [[],[],[],[2],[2],[2],[2],[]]
      -- .11XX22.
      (fmap (parseLines (fmap parseClaimDesc example1)) [Point (x, 3) | x <- [0..7]]) `shouldBe` [[],[1],[1],[2,1],[2,1],[2],[2],[]]

part1Spec :: Spec
part1Spec =
  describe "Day 3, Part1" $ do
    it "ff" $ do
      (part1 7 7 example1) `shouldBe` 4


