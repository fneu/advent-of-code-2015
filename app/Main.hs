module Main where

import Day01 (day01)
import Day02 (day02)
import DayTypes (Day (..))
import System.Environment (getArgs)

choose :: String -> Day -> (String -> String)
choose "2" = (.part2)
choose _ = (.part1)

runDay :: Day -> String -> String -> IO ()
runDay day part file = do
  let func = choose part day
  input <- readFile ("input/" ++ file)
  putStrLn $ func input

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["1", part, file] -> runDay day01 part file
    ["2", part, file] -> runDay day02 part file
    _ -> do
      putStrLn "Usage: program <day> <part> input/<file>"
      putStrLn "This day might not be implemented"
