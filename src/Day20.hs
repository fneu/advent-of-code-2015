module Day20 where

import Data.List (findIndex, nub)
import DayTypes (Day (..))

day20 :: Day
day20 = Day part1 part2

isqrt :: Int -> Int
isqrt = floor . sqrt . fromIntegral

factors :: Int -> [Int]
factors n = nub . concat $ [[x, q] | x <- [1 .. isqrt n], let (q, r) = divMod n x, r == 0]

presents :: Int -> Int
presents = (10 *) . sum . factors

presents2 :: Int -> Int
presents2 n = (11 *) . sum . filter (> (n - 1) `div` 50) . factors $ n

main :: IO ()
main = do
  print . findIndex (>= 34000000) . map presents2 $ [0 ..]

part1 :: String -> String
part1 input = show . findIndex (>= limit) . map presents $ [0 ..]
  where
    limit = read input

part2 :: String -> String
part2 input = show . findIndex (>= limit) . map presents2 $ [0 ..]
  where
    limit = read input
