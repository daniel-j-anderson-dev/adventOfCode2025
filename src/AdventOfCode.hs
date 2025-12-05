module AdventOfCode where

import Data.Maybe (mapMaybe)
import Text.Read (readMaybe)

input :: Word -> IO String
input n = readFile ("./input/day" ++ show n ++ ".txt")

data Puzzle = Part1 | Part2
