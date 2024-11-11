module Day14 where

import Control.Monad (join)
import DayTypes (Day (..))
import Text.Trifecta (CharParsing (string), Parser, Result (Failure, Success), integer, letter, many, whiteSpace)
import Text.Trifecta.Parser (parseString)

day14 :: Day
day14 = Day part1 part2

reindeer :: Parser [Integer]
reindeer = do
  _ <- many letter
  whiteSpace
  _ <- string "can fly "
  speed <- integer
  _ <- string "km/s for "
  sprintTime <- integer
  _ <- string "seconds, but then must rest for "
  restTime <- integer
  _ <- string "seconds."
  return . scanl (+) 0 . join . repeat $ (replicate (fromIntegral sprintTime) speed ++ replicate (fromIntegral restTime) 0)

part1 :: String -> String
part1 input = case mapM (parseString reindeer mempty) (lines input) of
  Failure e -> error $ "Could not parse reindeers: " ++ show e
  Success reindeers -> show . maximum $ map (!! 2503) reindeers

part2 :: String -> String
part2 input = case mapM (parseString reindeer mempty) (lines input) of
  Failure e -> error $ "Could not parse reindeers: " ++ show e
  Success reindeers -> show $ winningScore 2503 reindeers

advanceScore :: [(Int, [Integer])] -> [(Int, [Integer])]
advanceScore pairs =
  let lead = maximum $ map (head . snd) pairs
      advance (score, x : xs) = if x == lead then (score + 1, xs) else (score, xs)
      advance (_, []) = error "Reached end of an infinite reindeer, what. is. going. on."
   in map advance pairs

winningScore :: Int -> [[Integer]] -> Int
winningScore seconds reindeers =
  let pairs = map (0,) reindeers
      score s ps =
        if s == seconds
          then maximum $ map fst ps
          else score (s + 1) (advanceScore ps)
   in score 0 pairs - 1
