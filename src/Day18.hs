module Day18 where

import Data.List (nub, subsequences)
import DayTypes (Day (..))

day18 :: Day
day18 = Day part1 part2

type Pos = (Int, Int)

type Board = [Pos]

fullBoard :: Board
fullBoard = (,) <$> [0 .. 99] <*> [0 .. 99]

neighbors :: Pos -> [Pos]
neighbors (x, y) =
  [ (x - 1, y - 1),
    (x - 1, y),
    (x - 1, y + 1),
    (x, y - 1),
    (x, y + 1),
    (x + 1, y - 1),
    (x + 1, y),
    (x + 1, y + 1)
  ]

survivors :: Board -> [Pos]
survivors board = [pos | pos <- board, length (filter (`elem` board) (neighbors pos)) `elem` [2, 3]]

spawns :: Board -> [Pos]
spawns board = [pos | pos <- validDeadNeighbors, length (filter (`elem` board) (neighbors pos)) == 3]
  where
    deadNeighbors = filter (`notElem` board) (nub (concatMap neighbors board))
    validDeadNeighbors = filter (\(x, y) -> 0 <= x && x <= 99 && 0 <= y && y <= 99) deadNeighbors

step :: Board -> Board
step b = survivors b <> spawns b

part1 :: String -> String
part1 input = show . length $ iterate step initialBoard !! 100
  where
    inputLines = lines input
    initialBoard = filter (\(x, y) -> inputLines !! x !! y == '#') fullBoard

step2 :: Board -> Board
step2 b = nub $ survivors b <> spawns b <> [(0, 0), (0, 99), (99, 0), (99, 99)]

part2 :: String -> String
part2 input = show . length $ iterate step2 initialBoard !! 100
  where
    inputLines = lines input
    initialBoard = nub $ filter (\(x, y) -> inputLines !! x !! y == '#') fullBoard <> [(0, 0), (0, 99), (99, 0), (99, 99)]
