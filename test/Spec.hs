import qualified AdventOfCode (input)
import qualified AdventOfCode.Day1 (solution1, expectedSolution1)

main :: IO ()
main = do
  day1Input <- AdventOfCode.input 1

  let day1Part1Output = AdventOfCode.Day1.solution1 day1Input

  assertEqual "Day1.Part1" AdventOfCode.Day1.expectedSolution1 day1Part1Output

  pure ()

assertEqual :: Eq a => String -> a -> a -> IO ()
assertEqual message expected actual =
  if expected == actual
  then putStrLn $ message ++ ": PASSED"
  else error $ message ++ ": FAILED"
