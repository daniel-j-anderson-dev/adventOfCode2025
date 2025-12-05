import qualified AdventOfCode (input)
import AdventOfCode (Puzzle (Part1, Part2))
import qualified AdventOfCode.Day1 (solution, expected)

main :: IO ()
main = do
  day1Input <- AdventOfCode.input 1

  test
    "Day1.Part1"
    (AdventOfCode.Day1.expected Part1)
    (AdventOfCode.Day1.solution Part1 day1Input)

  test
    "Day1.Part2"
    (AdventOfCode.Day1.expected Part2)
    (AdventOfCode.Day1.solution Part2 day1Input)

  pure ()

test :: (Eq a, Show a) => String -> a -> a -> IO ()
test message expected actual =
  if expected == actual
    then putStrLn $ message ++ ": PASSED"
    else error $ message ++ ": FAILED\nexpected: " ++ show expected ++ "\nactual  : " ++ show actual ++ "\n"

