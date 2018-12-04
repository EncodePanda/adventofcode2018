module Main where

import qualified Day1 as D1
import qualified Day2 as D2
import qualified Day3 as D3

main :: IO ()
main = day3Part1Program "src/day3.input" >>= (putStrLn.show)

day1Part1Program :: FilePath -> IO Int
day1Part1Program = (fmap D1.part1).fetchInput

day1Part2Program :: FilePath -> IO Int
day1Part2Program = (fmap $ D1.part2).fetchInput

day2Part1Program :: FilePath -> IO Int
day2Part1Program path = (D2.part1.lines) <$> readFile path

day2Part2Program :: FilePath -> IO String
day2Part2Program path = ((D2.part2).lines) <$> readFile path

day3Part1Program :: FilePath -> IO Int
day3Part1Program path = ((D3.part1 999 999).lines) <$> readFile path

fetchInput :: FilePath -> IO [Int]
fetchInput path = do
  content <- readFile path
  return $ fmap line2Int (lines content)
    where
      line2Int ('+':v) = read v
      line2Int v = read v
