module Day3 where

import NanoParser
import Data.Maybe
data ClaimDesc = ClaimDesc Int Int Int Int Int
  deriving (Show, Eq)
newtype Point = Point (Int, Int)

type Id = Int
type Claim = Point -> [Id]

parseLine :: ClaimDesc -> Claim
parseLine desc point =
  (maybeToList . (within point)) desc
  where
    within :: Point -> ClaimDesc -> Maybe Id
    within (Point (x, y)) (ClaimDesc id le te w t)
      | x >= le && x < le + w && y >= te && y < te + t = Just id
      | otherwise = Nothing

parseLines :: [ClaimDesc] -> Claim
parseLines [] point = []
parseLines (h:t) point
  | length (parseLine h point) == 1 = head (parseLine h point) : parseLines t point
  | otherwise = parseLines t point

parseClaimDesc :: String -> ClaimDesc
parseClaimDesc line = runParser parser line
  where
    parser :: Parser ClaimDesc
    parser = do
      char '#'
      id <- natural
      spaces
      char '@'
      spaces
      le <- natural
      char ','
      te <- natural
      char ':'
      spaces
      w <- natural
      char 'x'
      t <- natural
      spaces
      pure $ ClaimDesc id le te w t

emptyFabric :: Int -> Int -> [Point]
emptyFabric w t = [Point (x, y) | x <- [0..w], y <- [0..t]]

part1 :: Int -> Int -> [String] -> Int
part1 w t lines  =
  foldl (+) 0 $ fmap found $ fabric
  where
    fabric = emptyFabric w t
    claim = parseLines (fmap parseClaimDesc lines)
    found point
      | claim point == [] = 0
      | drop 1 (claim point) == [] = 0
      | otherwise = 1

