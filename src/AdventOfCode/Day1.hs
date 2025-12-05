module AdventOfCode.Day1 where

import AdventOfCode (Puzzle (Part1, Part2))
import Data.Maybe (mapMaybe)
import Text.Read (readMaybe)

dialStart :: Int
dialStart = 50

dialSize :: Int
dialSize = 100

rotationFromString :: String -> Maybe Int
rotationFromString s = do
  direction <- case s of
    ('L' : _) -> Just (-1)
    ('R' : _) -> Just 1
    _ -> Nothing
  distance <- readMaybe (drop 1 s)
  Just (direction * distance)

parseInput :: String -> [Int]
parseInput input = mapMaybe rotationFromString (lines input)

solution :: Puzzle -> String -> Int
solution part input = fst ( foldl step (0, dialStart) (parseInput input))
  where
    step (zeroCount, dialPosition) offset = 
      let newDialPosition = applyRotation dialPosition offset
          newZeroCount = case part of
            Part1 -> case newDialPosition of
              0 -> succ zeroCount
              _ -> zeroCount
            Part2 -> zeroCount + zeroPasses dialPosition offset
       in (newZeroCount, newDialPosition)

zeroPasses :: Int -> Int -> Int
zeroPasses dialPosition offset
  | offset < 0 = newDialPosition `div` (-dialSize) - dialPosition `div` (-dialSize)
  | otherwise = newDialPosition `div` dialSize
  where
    newDialPosition = dialPosition + offset

applyRotation :: Int -> Int -> Int
applyRotation dialPosition offset = (dialPosition + offset) `mod` dialSize

expected :: Puzzle -> Int
expected Part1 = 1152
expected Part2 = 6671
