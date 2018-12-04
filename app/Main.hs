module Main where

import qualified Day1 as D1
import qualified Day2 as D2
import qualified Day3 as D3

main :: IO ()
main = do
  -- ((fmap D1.part1).fetchInput) "src/day1.input" >>= (putStrLn.show)
  -- ((fmap D1.part2).fetchInput) "src/day1.input" >>= (putStrLn.show)
  -- (D2.part1.lines) <$> readFile "src/day2.input" >>= (putStrLn.show)
  -- (D2.part2.lines) <$> readFile "src/day2.input" >>= (putStrLn.show)
  ((D3.part1 999 999).lines) <$> readFile "src/day2.input" >>= (putStrLn.show)

fetchInput :: FilePath -> IO [Int]
fetchInput path = do
  content <- readFile path
  return $ fmap line2Int (lines content)
    where
      line2Int ('+':v) = read v
      line2Int v = read v
