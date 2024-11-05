module Day10 where

import DayTypes (Day (..))

next :: String -> String
next s = go 0 s
  where
    go :: Int -> String -> String
    go _ [] = ""
    go n [c] = (show $ n + 1) ++ [c]
    go n (x : y : xs) =
      if x == y
        then go (n + 1) (y : xs)
        else (show $ n + 1) ++ [x] ++ go 0 (y : xs)

day10 :: Day
day10 = Day part1 part2

part1 :: String -> String
part1 input = show . length $ iterate next clean !! 40
  where
    clean = takeWhile (/= '\n') input

part2 :: String -> String
part2 input = show . length $ iterate next clean !! 50
  where
    clean = takeWhile (/= '\n') input
