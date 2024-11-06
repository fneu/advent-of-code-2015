module Day13 (day13) where

import Data.List (nub, permutations)
import Data.Map qualified as M
import DayTypes (Day (..))

parseHappiness :: M.Map (String, String) Int -> String -> M.Map (String, String) Int
parseHappiness m line =
  case words line of
    [subject, "would", "gain", units, "happiness", "units", "by", "sitting", "next", "to", neighbor] -> M.insert (subject, init neighbor) (read units) m
    [subject, "would", "lose", units, "happiness", "units", "by", "sitting", "next", "to", neighbor] -> M.insert (subject, init neighbor) (-(read units)) m
    _ -> error $ "Cannot parse line: " ++ line

deltaHappiness :: M.Map (String, String) Int -> [String] -> Int
deltaHappiness sympathyMap people = go sympathyMap (head people) people
  where
    go _ _ [] = error "We shouldn't rean an empty list"
    go m h [end] = m M.! (h, end) + m M.! (end, h)
    go m h (x : y : xs) = m M.! (x, y) + m M.! (y, x) + go m h (y : xs)

day13 :: Day
day13 = Day part1 part2

part1 :: String -> String
part1 input = show $ maximum totals
  where
    sympathyMap = foldl parseHappiness M.empty (lines input)
    people = nub $ map fst (M.keys sympathyMap)
    arrangements = map ([head people] <>) (permutations . tail $ people)
    totals = map (deltaHappiness sympathyMap) arrangements

-- no wrap around
deltaHappiness2 :: M.Map (String, String) Int -> [String] -> Int
deltaHappiness2 _ [] = 0
deltaHappiness2 _ [_] = 0
deltaHappiness2 m (x : y : xs) = m M.! (x, y) + m M.! (y, x) + deltaHappiness2 m (y : xs)

-- now it matters who's first
part2 :: String -> String
part2 input = show $ maximum totals
  where
    sympathyMap = foldl parseHappiness M.empty (lines input)
    people = nub $ map fst (M.keys sympathyMap)
    arrangements = permutations people
    totals = map (deltaHappiness2 sympathyMap) arrangements
