module AdventOfCode.Day1 where

import Data.Maybe (mapMaybe)
import Text.Read (readMaybe)

dialStart :: Word
dialStart = 50

dialSize :: Word
dialSize = 100

expectedSolution1 :: Word
expectedSolution1 = 1152

data Direction = L | R

directionFromString :: String -> Maybe Direction
directionFromString s = case s of
  ('L' : _) -> Just L
  ('R' : _) -> Just R
  _ -> Nothing

data Rotation = Rotation Direction Word

offset :: Rotation -> Word
offset (Rotation direction distance) = case direction of
  L -> dialSize - (distance `mod` dialSize)
  R -> distance

rotationFromString :: String -> Maybe Rotation
rotationFromString s = do
  direction <- directionFromString s
  distance <- readMaybe (drop 1 s)
  Just (Rotation direction distance)

parseInput :: String -> [Rotation]
parseInput input = mapMaybe rotationFromString (lines input)

solution1 :: String -> Word
solution1 input = fst (foldl
  (\(zeroCount, dialPosition) rotation ->
    let newDialPosition = (dialPosition + (offset rotation)) `mod` dialSize
        newZeroCount = case newDialPosition of
          0 -> zeroCount + 1
          _ -> zeroCount
     in (newZeroCount, newDialPosition)
  )
  (0, dialStart)
  (parseInput input))
