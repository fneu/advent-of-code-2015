module Day17 where

import Data.List (subsequences)
import DayTypes (Day (..))

day17 :: Day
day17 = Day part1 part2

part1 :: String -> String
part1 input = show . length $ filter (== 150) (map sum (subsequences buckets))
  where
    buckets :: [Int] = map read (lines input)

part2 :: String -> String
part2 input = show . length $ shortCombinations
  where
    buckets :: [Int] = map read (lines input)
    validCombinations = filter (\subseq -> sum subseq == 150) (subsequences buckets)
    minLength = minimum $ map length validCombinations
    shortCombinations = filter (\subseq -> length subseq == minLength) validCombinations
