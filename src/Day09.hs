module Day09 where

import Data.List (nub, permutations)
import Data.Map qualified as M
import DayTypes (Day (..))

day09 :: Day
day09 = Day part1 part2

addLine :: M.Map (String, String) Int -> String -> M.Map (String, String) Int
addLine m s = case words s of
  [from, "to", to, "=", distance] -> m''
    where
      dist = read distance
      m' = M.insert (from, to) dist m
      m'' = M.insert (to, from) dist m'
  _ -> error $ "cannot parse line: " ++ s

addDistance :: M.Map (String, String) Int -> [String] -> Int
addDistance _ [] = 0
addDistance _ [_] = 0
addDistance m (x : y : xs) = case M.lookup (x, y) m of
  Nothing -> error $ "Could not find distance for " ++ show (x, y)
  Just dist -> dist + addDistance m (y : xs)

part1 :: String -> String
part1 input = show . minimum $ distances
  where
    distanceMap = foldl addLine M.empty (lines input)
    cities = nub $ map fst (M.keys distanceMap)
    connections = permutations cities
    distances = map (addDistance distanceMap) connections

part2 :: String -> String
part2 input = show . maximum $ distances
  where
    distanceMap = foldl addLine M.empty (lines input)
    cities = nub $ map fst (M.keys distanceMap)
    connections = permutations cities
    distances = map (addDistance distanceMap) connections
