module Main where

import Day1

main :: IO ()
main = part1Program "src/day1part1.input" >>= (putStrLn.show)

part1Program :: FilePath -> IO Int
part1Program = (fmap part1).fetchInput

fetchInput :: FilePath -> IO [Int]
fetchInput path = do
  content <- readFile path
  return $ fmap line2Int (lines content)
    where
      line2Int ('+':v) = read v
      line2Int v = read v
