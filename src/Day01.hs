module Day01 (day01) where

import DayTypes (Day (..))

day01 :: Day
day01 = Day part1 part2

part1 :: String -> String
part1 = show . sum . map f
  where
    f c = case c of
      '(' -> 1 :: Int
      ')' -> -1 :: Int
      _ -> 0 :: Int

part2 :: String -> String
part2 s = show . length $ takeWhile (>= 0) $ scanl (+) 0 (map f s)
  where
    f c = case c of
      '(' -> 1 :: Int
      ')' -> -1 :: Int
      _ -> 0 :: Int
