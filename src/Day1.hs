module Day1 where

type Input = String

part1 :: Input -> Int
part1 = sum.(fmap line2Int).lines
  where
    line2Int ('+':v) = read v
    line2Int v = read v
