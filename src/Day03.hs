module Day03 where

import Data.List (nub)
import DayTypes (Day (..))

day03 :: Day
day03 = Day part1 part2

move :: (Int, Int) -> Char -> (Int, Int)
move (x, y) '^' = (x + 1, y)
move (x, y) 'v' = (x - 1, y)
move (x, y) '>' = (x, y + 1)
move (x, y) '<' = (x, y - 1)
move _ c = error $ "unknown move" ++ [c]

part1 :: String -> String
part1 input = show . length $ houses
  where
    houses = nub $ scanl move (0, 0) input

evenIndices :: [a] -> [a]
evenIndices xs = [x | (x, i) <- zip xs ([0 ..] :: [Int]), even i]

oddIndices :: [a] -> [a]
oddIndices xs = [x | (x, i) <- zip xs ([0 ..] :: [Int]), odd i]

part2 :: String -> String
part2 input = show . length $ houses
  where
    santaHouses = scanl move (0, 0) (evenIndices input)
    roboHouses = scanl move (0, 0) (oddIndices input)
    houses = nub $ santaHouses <> roboHouses
