module Day25 where

import DayTypes (Day (..))

day25 :: Day
day25 = Day part1 part2

parseInput :: String -> (Int, Int)
parseInput input =
  let row = read $ init ((words input) !! 15)
      column = read $ init ((words input) !! 17)
   in (row, column)

index :: (Int, Int) -> Int
index (r, c) =
  let k = c + r - 1
      start = (k * (k - 1)) `div` 2 + 1
      offset = c - 1
   in start + offset - 1

part1 :: String -> String
part1 input = show $ numbers !! i
  where
    coords = parseInput input
    i = index coords
    numbers :: [Integer]
    numbers = iterate (\n -> (n * 252533) `rem` 33554393) 20151125

part2 :: String -> String
part2 _ = "this one comes for free"
