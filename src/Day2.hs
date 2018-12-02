module Day2 where

import Data.Foldable
import qualified Data.Map.Strict as Map

part1 :: [String] -> Int
part1 =
  uncurry (*)
  . foldl sumTuple (0, 0)
  . fmap counts
  where
    counts :: String -> (Int, Int)
    counts =
      (\l -> ((bool2Int.elem 2) l, (bool2Int.elem 3) l))
      . Map.elems
      . foldl (Map.unionWith (+)) Map.empty
      . fmap (flip Map.singleton $ 1)

    bool2Int True = 1
    bool2Int False = 0

    sumTuple (a1, b1) (a2, b2) = (a1+a2, b1+b2)
