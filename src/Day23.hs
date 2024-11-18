module Day23 where

import Control.Applicative ((<|>))
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import DayTypes (Day (..))
import Text.Trifecta (Parser, Result (..), char, integer, parseString, string, whiteSpace)

day23 :: Day
day23 = Day part1 part2

data Register = A | B deriving (Show)

data Instruction = HLF Register | TPL Register | INC Register | JMP Integer | JIE Register Integer | JIO Register Integer | ABORT deriving (Show)

data MachineState = MachineState Integer Integer Integer Bool deriving (Show)

parseRegister :: Parser Register
parseRegister = (char 'a' >> return A) <|> (char 'b' >> return B)

parseHLF :: Parser Instruction
parseHLF = do
  _ <- string "hlf "
  HLF <$> parseRegister

parseTPL :: Parser Instruction
parseTPL = do
  _ <- string "tpl "
  TPL <$> parseRegister

parseINC :: Parser Instruction
parseINC = do
  _ <- string "inc "
  INC <$> parseRegister

parseJMP :: Parser Instruction
parseJMP = do
  _ <- string "jmp "
  JMP <$> integer

parseJIE :: Parser Instruction
parseJIE = do
  _ <- string "jie "
  r <- parseRegister
  _ <- char ','
  whiteSpace
  JIE r <$> integer

parseJIO :: Parser Instruction
parseJIO = do
  _ <- string "jio "
  r <- parseRegister
  _ <- char ','
  whiteSpace
  JIO r <$> integer

parseInstruction :: Parser Instruction
parseInstruction = parseHLF <|> parseTPL <|> parseINC <|> parseJMP <|> parseJIE <|> parseJIO

parseInstructions :: String -> M.Map Integer Instruction
parseInstructions input =
  let instructions = map (parseString parseInstruction mempty) (lines input)
   in M.fromList $ zip [0 ..] (map replaceFailures instructions)
  where
    replaceFailures result = case result of
      Success instruction -> instruction
      Failure _ -> ABORT

advance :: M.Map Integer Instruction -> MachineState -> MachineState
advance _ (MachineState _ _ _ True) = error "Cannot advance aborted state"
advance instructions (MachineState position a b _) =
  case fromMaybe ABORT (M.lookup position instructions) of
    ABORT -> MachineState position a b True
    HLF A -> MachineState (position + 1) (a `div` 2) b False
    HLF B -> MachineState (position + 1) a (b `div` 2) False
    TPL A -> MachineState (position + 1) (a * 3) b False
    TPL B -> MachineState (position + 1) a (b * 3) False
    INC A -> MachineState (position + 1) (a + 1) b False
    INC B -> MachineState (position + 1) a (b + 1) False
    JMP i -> MachineState (position + i) a b False
    JIE A i ->
      if even a
        then MachineState (position + i) a b False
        else MachineState (position + 1) a b False
    JIE B i ->
      if even b
        then MachineState (position + i) a b False
        else MachineState (position + 1) a b False
    JIO A i ->
      if a == 1
        then MachineState (position + i) a b False
        else MachineState (position + 1) a b False
    JIO B i ->
      if b == 1
        then MachineState (position + i) a b False
        else MachineState (position + 1) a b False

running :: MachineState -> Bool
running (MachineState _ _ _ aborted) = not aborted

part1 :: String -> String
part1 input = show . head . dropWhile running $ states
  where
    instructions = parseInstructions input
    states = iterate (advance instructions) (MachineState 0 0 0 False)

part2 :: String -> String
part2 input = show . head . dropWhile running $ states
  where
    instructions = parseInstructions input
    states = iterate (advance instructions) (MachineState 0 1 0 False)
