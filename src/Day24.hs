module Day24 where

import DayTypes (Day (..))

combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations _ [] = []
combinations n (x : xs) =
  map (x :) (combinations (n - 1) xs) ++ combinations n xs

day24 :: Day
day24 = Day part1 part2

part1 :: String -> String
part1 input = show $ minimum $ map product smallestGroups
  where
    packages :: [Int]
    packages = map read (lines input)
    groupSize = sum packages `div` 3
    smallestGroups = head $ dropWhile (== []) $ map (\n -> filter (\ps -> sum ps == groupSize) (combinations n packages)) [1 ..]

part2 :: String -> String
part2 input = show $ minimum $ map product smallestGroups
  where
    packages :: [Int]
    packages = map read (lines input)
    groupSize = sum packages `div` 4
    smallestGroups = head $ dropWhile (== []) $ map (\n -> filter (\ps -> sum ps == groupSize) (combinations n packages)) [1 ..]
