module Day08 where

import DayTypes (Day (..))

day08 :: Day
day08 = Day part1 part2

doubleSlashes :: String -> (Int, String)
doubleSlashes str =
  let (_, number, cleaned) = doubleSlashes' ("", 0, str)
   in (number, cleaned)
  where
    doubleSlashes' :: (String, Int, String) -> (String, Int, String)
    doubleSlashes' (s, n, "") = ("", n, s)
    doubleSlashes' (s, n, [c]) = ("", n, s ++ [c])
    doubleSlashes' (s, n, x : y : xs) =
      if x == '\\' && y == '\\'
        then doubleSlashes' (s, n + 1, xs)
        else doubleSlashes' (s ++ [x], n, y : xs)

escapedQuotes :: String -> Int
escapedQuotes [] = 0
escapedQuotes [_] = 0
escapedQuotes (x : y : xs) =
  if x == '\\' && y == '"'
    then 1 + escapedQuotes xs
    else escapedQuotes (y : xs)

hexa :: String -> Int
hexa [] = 0
hexa [_] = 0
hexa [_, _] = 0
hexa [_, _, _] = 0
hexa (x : y : z : w : xs) =
  if x == '\\'
    && y == 'x'
    && z `elem` ("0123456789abcdef" :: String)
    && w `elem` ("0123456789abcdef" :: String)
    then 3 + hexa xs
    else hexa (y : z : w : xs)

countExtra :: String -> Int
countExtra input = 2 + slashes + quotes + hexs
  where
    (slashes, cleaned) = doubleSlashes input
    quotes = escapedQuotes cleaned
    hexs = hexa cleaned

part1 :: String -> String
part1 input = show $ sum $ map countExtra (lines input)

countSlashes :: String -> Int
countSlashes s = length $ filter (== '\\') s

countQuotes :: String -> Int
countQuotes s = length $ filter (== '"') s

necessaryDelta :: String -> Int
necessaryDelta s = 2 + countSlashes s + countQuotes s

part2 :: String -> String
part2 input = show $ sum $ map necessaryDelta (lines input)
