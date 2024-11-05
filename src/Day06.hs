{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

{-# HLINT ignore "Use <$>" #-}

module Day06 (day06) where

import Control.Applicative ((<|>))
import Data.Functor (($>))
import DayTypes (Day (..))
import Text.Trifecta (Parser, Result (..), char, integer, parseString, string, try)

day06 :: Day
day06 = Day part1 part2

data InstructionType = On | Off | Toggle deriving (Show, Eq)

data Instruction = Instruction
  { instructionType :: InstructionType,
    minx :: Integer,
    miny :: Integer,
    maxx :: Integer,
    maxy :: Integer
  }
  deriving (Show)

switchOn :: Parser InstructionType
switchOn = try (string "turn on" $> On)

switchOff :: Parser InstructionType
switchOff = try (string "turn off" $> Off)

toggle :: Parser InstructionType
toggle = try (string "toggle" $> Toggle)

instruction :: Parser Instruction
instruction = do
  s <- switchOn <|> switchOff <|> toggle
  char ' '
  minx <- integer
  char ','
  miny <- integer
  string "through "
  maxx <- integer
  char ','
  maxy <- integer
  return $ Instruction s minx miny maxx maxy

toggleState :: Integer -> Integer -> [Instruction] -> Integer
toggleState x y = foldl (adjust x y) 0
  where
    adjust x' y' current cmd
      | cmd.miny > y' = current
      | cmd.maxy < y' = current
      | cmd.minx > x' = current
      | cmd.maxx < x' = current
      | otherwise = case cmd.instructionType of
          On -> 1
          Off -> 0
          Toggle -> 1 - current

part1 :: String -> String
part1 input = case commands of
  Success cs -> show $ sum $ lights cs
  Failure e -> "Unable to parse commands: " ++ show e
  where
    commands = mapM (parseString instruction mempty) (lines input)
    lights cs = [toggleState x y cs | x <- [0 .. 999], y <- [0 .. 999]]

brightness :: Integer -> Integer -> [Instruction] -> Integer
brightness x y = foldl (adjust x y) 0
  where
    adjust x' y' current cmd
      | cmd.miny > y' = current
      | cmd.maxy < y' = current
      | cmd.minx > x' = current
      | cmd.maxx < x' = current
      | otherwise = case cmd.instructionType of
          On -> current + 1
          Off -> max 0 (current - 1)
          Toggle -> current + 2

part2 :: String -> String
part2 input = case commands of
  Success cs -> show $ sum $ lights cs
  Failure e -> "Unable to parse commands: " ++ show e
  where
    commands = mapM (parseString instruction mempty) (lines input)
    lights cs = [brightness x y cs | x <- [0 .. 999], y <- [0 .. 999]]
