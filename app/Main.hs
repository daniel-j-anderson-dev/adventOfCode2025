module Main where

import Lib


main :: IO ()
main = do
  day1Input <- input 1
  let x = mapMaybe (rotationFromString) (lines day1Input)
  print x
  pure ()
