{-# LANGUAGE OverloadedStrings #-}

module AdventOfCode.Day1 where

import AdventOfCode (Puzzle (Part1, Part2), parseIntegralBase10)
import Data.Maybe (mapMaybe)
import qualified Data.Text as T

dialStart :: Int
dialStart = 50

dialSize :: Int
dialSize = 100

parseLine :: T.Text -> Maybe Int
parseLine s = do
  (direction, n) <- case T.uncons s of
    Just ('L', n) -> Just (-1, n)
    Just ('R', n) -> Just (1, n)
    _ -> Nothing
  distance <- parseIntegralBase10 n
  Just (direction * distance)

parseInput :: T.Text -> [Int]
parseInput input = mapMaybe parseLine (T.lines input)

zeroPasses :: Int -> Int -> Int
zeroPasses dialPosition offset
  | offset < 0 = newDialPosition `div` (-dialSize) - dialPosition `div` (-dialSize)
  | otherwise = newDialPosition `div` dialSize
  where
    newDialPosition = dialPosition + offset

applyRotation :: Int -> Int -> Int
applyRotation dialPosition offset = (dialPosition + offset) `mod` dialSize

solution :: Puzzle -> T.Text -> Int
solution part input = fst (foldl step (0, dialStart) (parseInput input))
  where
    step (zeroCount, dialPosition) offset = (newZeroCount, newDialPosition)
      where
        newDialPosition = applyRotation dialPosition offset
        newZeroCount = case part of
          Part1 -> case newDialPosition of
            0 -> succ zeroCount
            _ -> zeroCount
          Part2 -> zeroCount + zeroPasses dialPosition offset

expected :: Puzzle -> Int
expected Part1 = 1152
expected Part2 = 6671
