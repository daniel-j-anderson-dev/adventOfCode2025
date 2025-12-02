module Lib where

import Data.Maybe (mapMaybe)
import Text.Read (readMaybe)

input :: Word -> IO String
input n = readFile ("./input/day" ++ show n ++ ".txt")

dialStart = 50

dialSize = 100

data Direction = L | R
  deriving (Show)

directionFromString :: String -> Maybe Direction
directionFromString s = case s of
  ('L' : _) -> Just L
  ('R' : _) -> Just R
  _ -> Nothing

data Rotation = Rotation Direction Word
  deriving (Show)

rotationFromString :: String -> Maybe Rotation
rotationFromString s = do
  direction <- directionFromString s
  distance <- readMaybe (tail s)
  Just (Rotation direction distance)

parseInput :: String -> [Rotation]
parseInput input = mapMaybe (rotationFromString) (lines input)
