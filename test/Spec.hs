{-# LANGUAGE OverloadedStrings #-}

import qualified AdventOfCode (input)
import AdventOfCode (Puzzle (Part1, Part2))
import qualified AdventOfCode.Day1 (solution, expected)
import qualified AdventOfCode.Day2 (parseInput)
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO

main :: IO ()
main = do
  day1Input <- AdventOfCode.input 1
  day2Input <- AdventOfCode.input 2

  test
    "Day1.Part1"
    (AdventOfCode.Day1.expected Part1)
    (AdventOfCode.Day1.solution Part1 day1Input)

  test
    "Day1.Part2"
    (AdventOfCode.Day1.expected Part2)
    (AdventOfCode.Day1.solution Part2 day1Input)

  T.IO.putStrLn $ T.show (AdventOfCode.Day2.parseInput day2Input)

  pure ()

test :: (Eq a, Show a) => T.Text -> a -> a -> IO ()
test message expected actual =
  if expected == actual
    then T.IO.putStrLn $ message <> ": PASSED"
    else error (T.unpack (message <> ": FAILED\nexpected: " <> T.show expected <> "\nactual  : " <> T.show actual <> "\n"))

