{-# LANGUAGE OverloadedStrings #-}

module AdventOfCode where

import qualified Data.Text as T
import qualified Data.Text.IO as T.IO
import qualified Data.Text.Read as T.R

inputPath :: Word -> FilePath
inputPath n = "./input/day" ++ show n ++ ".txt" 

input :: Word -> IO T.Text
input n = T.IO.readFile (inputPath n)

data Puzzle = Part1 | Part2

eitherToMaybe :: Either err ok -> Maybe ok
eitherToMaybe = either (const Nothing) Just

parseIntegralBase10 :: Integral a => T.Text -> Maybe a
parseIntegralBase10 s = case T.R.decimal s of
  Right (n, unparsed) | T.null unparsed -> Just n
  _ -> Nothing
