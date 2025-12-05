{-# LANGUAGE OverloadedStrings #-}

module AdventOfCode.Day2 where

-- import AdventOfCode (Puzzle (Part1, Part2), parseIntegralBase10)
import qualified Data.Text as T

ranges :: T.Text -> [T.Text]
ranges = T.split (== ',')

ids :: T.Text -> [T.Text]
ids = T.split (== '-')

parseInput :: T.Text -> [[T.Text]]
parseInput input = map ids (ranges input)
