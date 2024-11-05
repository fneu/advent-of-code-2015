module Day05 where

import DayTypes (Day (..))

threeVowels :: String -> Bool
threeVowels = go 0
  where
    go :: Int -> String -> Bool
    go 3 _ = True
    go _ [] = False
    go n (x : xs) =
      if x `elem` ("aeiou" :: String)
        then go (n + 1) xs
        else go n xs

hasDouble :: String -> Bool
hasDouble (x : y : _) | x == y = True
hasDouble (_ : xs) = hasDouble xs
hasDouble [] = False

hasForbidden :: String -> Bool
hasForbidden ('a' : 'b' : _) = True
hasForbidden ('c' : 'd' : _) = True
hasForbidden ('p' : 'q' : _) = True
hasForbidden ('x' : 'y' : _) = True
hasForbidden (_ : xs) = hasForbidden xs
hasForbidden [] = False

isNice :: String -> Bool
isNice s = threeVowels s && hasDouble s && (not $ hasForbidden s)

day05 :: Day
day05 = Day part1 part2

part1 :: String -> String
part1 input = show $ length $ filter isNice (lines input)

hasSandwich :: String -> Bool
hasSandwich (x : _ : z : _) | x == z = True
hasSandwich (_ : xs) = hasSandwich xs
hasSandwich [] = False

hasDoublePair :: String -> Bool
hasDoublePair [] = False
hasDoublePair [_] = False
hasDoublePair (x : y : xs) =
  case hasPair x y xs of
    True -> True
    False -> hasDoublePair (y : xs)
  where
    hasPair _ _ [] = False
    hasPair _ _ [_] = False
    hasPair a b (a' : b' : as)
      | a == a' && b == b' = True
      | otherwise = hasPair a b (b' : as)

isNice2 :: String -> Bool
isNice2 s = hasSandwich s && hasDoublePair s

part2 :: String -> String
part2 input = show $ length $ filter isNice2 (lines input)
