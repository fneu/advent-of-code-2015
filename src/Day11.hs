module Day11 where

import Data.Char (chr, ord)
import DayTypes (Day (..))

fromBase26 :: String -> Int
fromBase26 = foldl (\acc x -> acc * 26 + (ord x - ord 'a' + 1)) 0

toBase26 :: Int -> String
toBase26 0 = "a"
toBase26 n = reverse (convert n)
  where
    convert 0 = []
    convert x =
      let (q, r) = (x - 1) `divMod` 26
       in chr (ord 'a' + r) : convert q

hasStaircase :: String -> Bool
hasStaircase s =
  ord (s !! 0) + 1 == ord (s !! 1) && ord (s !! 1) + 1 == ord (s !! 2)
    || ord (s !! 1) + 1 == ord (s !! 2) && ord (s !! 2) + 1 == ord (s !! 3)
    || ord (s !! 2) + 1 == ord (s !! 3) && ord (s !! 3) + 1 == ord (s !! 4)
    || ord (s !! 3) + 1 == ord (s !! 4) && ord (s !! 4) + 1 == ord (s !! 5)
    || ord (s !! 4) + 1 == ord (s !! 5) && ord (s !! 5) + 1 == ord (s !! 6)
    || ord (s !! 5) + 1 == ord (s !! 6) && ord (s !! 6) + 1 == ord (s !! 7)

hasNoForbiddens :: String -> Bool
hasNoForbiddens s =
  'i' `notElem` s
    && 'o' `notElem` s
    && 'l' `notElem` s

hasTwoPairs :: String -> Bool
hasTwoPairs [] = False
hasTwoPairs [_] = False
hasTwoPairs (x : y : xs) =
  if x == y
    then hasPair xs
    else hasTwoPairs (y : xs)
  where
    hasPair [] = False
    hasPair [_] = False
    hasPair (x' : y' : xs') =
      if x' == y'
        then True
        else hasPair (y' : xs')

isPassword :: String -> Bool
isPassword s =
  hasStaircase s
    && hasNoForbiddens s
    && hasTwoPairs s

next :: Int -> Int
next i =
  if isPassword (toBase26 (i + 1))
    then i + 1
    else next (i + 1)

day11 :: Day
day11 = Day part1 part2

part1 :: String -> String
part1 input = show . toBase26 $ next initial
  where
    clean = takeWhile (/= '\n') input
    initial = fromBase26 clean

part2 :: String -> String
part2 input = show . toBase26 $ next (next initial)
  where
    clean = takeWhile (/= '\n') input
    initial = fromBase26 clean
