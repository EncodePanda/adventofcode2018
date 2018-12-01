module Main where

import Day1

main :: IO ()
main = part2Program "src/day1.input" >>= (putStrLn.show)

part1Program :: FilePath -> IO Int
part1Program = (fmap part1).fetchInput

part2Program :: FilePath -> IO Int
part2Program = (fmap $ part2).fetchInput

fetchInput :: FilePath -> IO [Int]
fetchInput path = do
  content <- readFile path
  return $ fmap line2Int (lines content)
    where
      line2Int ('+':v) = read v
      line2Int v = read v
