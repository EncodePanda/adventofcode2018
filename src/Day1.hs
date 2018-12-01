module Day1 where

import qualified Data.Set as Set

part1 :: [Int] -> Int
part1 = sum

part2 :: [Int] -> Int
part2 = (find Set.empty) . scanl (+) 0 . cycle
  where
    find :: Set.Set Int -> [Int] -> Int
    find set [] = error "no results"
    find set (h:tail) = if Set.member h set then h
                        else (find (Set.insert h set) tail)
