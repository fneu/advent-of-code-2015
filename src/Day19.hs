module Day19 where

import Data.List (foldl', nub, sortBy)
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)
import Data.Text qualified as T
import Data.Text.Internal.Search
import DayTypes (Day (..))

day19 :: Day
day19 = Day part1 part2

parsePair :: T.Text -> (T.Text, T.Text)
parsePair input =
  let ws = T.words input
   in (head ws, ws !! 2)

molecules :: M.Map T.Text [T.Text] -> T.Text -> [T.Text]
molecules m str
  | T.length str == 0 = []
  | T.length str == 1 = fromMaybe [T.take 1 str] (M.lookup (T.take 1 str) m)
  | otherwise = case M.lookup (T.take 2 str) m of
      Just rs -> [T.concat [start, T.drop 2 str] | start <- rs] <> map (\s -> T.concat [T.take 2 str, s]) (molecules m (T.drop 2 str))
      Nothing -> case M.lookup (T.take 1 str) m of
        Just rs -> [T.concat [start, T.drop 1 str] | start <- rs] <> map (\s -> T.concat [T.take 1 str, s]) (molecules m (T.tail str))
        Nothing -> map (\s -> T.concat [T.take 1 str, s]) (molecules m (T.tail str))

part1 :: String -> String
part1 input =
  let inputLines = lines input
      pairLines = drop 2 $ reverse inputLines
      pairs = map (parsePair . T.pack) pairLines
      pairMap = foldl' (\acc (key, value) -> M.insertWith (++) key [value] acc) M.empty pairs
      startMolecule = T.pack . last $ inputLines
   in show . length . nub $ molecules pairMap startMolecule

parsePair2 :: T.Text -> (T.Text, T.Text)
parsePair2 input =
  let ws = T.words input
   in (ws !! 2, head ws)

replaceAtIndices :: Int -> Int -> T.Text -> T.Text -> T.Text
replaceAtIndices start end replacement text =
  let prefix = T.take start text
      suffix = T.drop end text
   in T.concat [prefix, replacement, suffix]

molecules2 :: [(T.Text, T.Text)] -> T.Text -> [T.Text]
molecules2 pairs input = concatMap (replaceAll input) pairs
  where
    replaceAll :: T.Text -> (T.Text, T.Text) -> [T.Text]
    replaceAll molecule (from, to) =
      let l = T.length from
          startingIndices = indices from molecule
       in map (\start -> replaceAtIndices start (start + l) to molecule) startingIndices

part2 :: String -> String
part2 input =
  let inputLines = lines input
      pairLines = drop 2 $ reverse inputLines
      pairs = map (parsePair2 . T.pack) pairLines
      endMolecule = T.pack . last $ inputLines
      iterations = iterate (take 50 . sortBy (comparing T.length) . nub . concatMap (molecules2 pairs)) [endMolecule]
   in show . length $ takeWhile ("e" `notElem`) iterations
