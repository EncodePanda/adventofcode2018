module Main where

import Day1

main :: IO ()
main = part1Program "src/day1part1.input"

part1Program :: FilePath -> IO ()
part1Program path = (part1 <$> (readFile path)) >>= (putStrLn.show)
