module Day2 where

import Data.Maybe
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


diffBy1 :: String -> String -> Bool
diffBy1 = diffBy1' Nothing  
  where
    diffBy1' :: Maybe Char -> String -> String ->  Bool
    diffBy1' Nothing [] []  = False
    diffBy1' (Just c) [] []  = True
    diffBy1' Nothing (h1:t1) (h2:t2)  =
      if h1 == h2 then diffBy1' Nothing t1 t2  else diffBy1' (Just h1) t1 t2 
    diffBy1' (Just c) (h1:t1) (h2:t2)  =
      if h1 == h2 then diffBy1' (Just c)  t1 t2  else False
    diffBy1' _ _ _ = False

findSimilar :: [String] -> (String, String)
findSimilar ss = head (ss >>= (check ss))
  where
    check (s1:t) s2 = if diffBy1 s1 s2 then [(s1, s2)] else check t s2
    check [] s2 = []

common :: String -> String -> String
common s1 s2 = reverse $ common' [] s1 s2
  where
    common' :: String -> String -> String -> String
    common' acc [] [] = acc
    common' acc (h1:t1) (h2:t2) =
      if h1 == h2 then common' (h1:acc) t1 t2
      else common' acc t1 t2
    

part2 :: [String] -> String
part2 = uncurry common .findSimilar

