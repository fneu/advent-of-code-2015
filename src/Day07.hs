{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

{-# HLINT ignore "Use <$>" #-}

module Day07 (day07) where

import Control.Applicative ((<|>))
import Data.Bits (complement, shiftL, shiftR, (.&.), (.|.))
import Data.Map qualified as M
import Data.Word (Word16)
import DayTypes (Day (..))
import Text.Trifecta (Parser, Result (..), integer, lower, many, parseString, string, try, whiteSpace)

day07 :: Day
day07 = Day part1 part2

data Input = Value Word16 | Wire String | GateRef Gate
  deriving (Show)

data Gate
  = AND Input Input
  | LSHIFT Input Input
  | NOT Input
  | OR Input Input
  | RSHIFT Input Input
  deriving (Show)

data Instruction = Instruction
  { source :: Input,
    sink :: String
  }
  deriving (Show)

value :: Parser Input
value = do
  i <- integer
  return $ Value (fromIntegral i)

wire :: Parser Input
wire = do
  s <- many lower
  return $ Wire s

andGate :: Parser Gate
andGate = do
  i1 <- value <|> wire
  whiteSpace
  string "AND"
  whiteSpace
  i2 <- value <|> wire
  return $ AND i1 i2

lshiftGate :: Parser Gate
lshiftGate = do
  i1 <- value <|> wire
  whiteSpace
  string "LSHIFT"
  whiteSpace
  i2 <- value <|> wire
  return $ LSHIFT i1 i2

rshiftGate :: Parser Gate
rshiftGate = do
  i1 <- value <|> wire
  whiteSpace
  string "RSHIFT"
  whiteSpace
  i2 <- value <|> wire
  return $ RSHIFT i1 i2

orGate :: Parser Gate
orGate = do
  i1 <- value <|> wire
  whiteSpace
  string "OR"
  whiteSpace
  i2 <- value <|> wire
  return $ OR i1 i2

notGate :: Parser Gate
notGate = do
  string "NOT"
  whiteSpace
  i1 <- value <|> wire
  return $ NOT i1

gate :: Parser Input
gate = do
  g <- try andGate <|> try orGate <|> try notGate <|> try lshiftGate <|> try rshiftGate
  return $ GateRef g

source :: Parser Input
source = gate <|> value <|> wire

instruction :: Parser Instruction
instruction = do
  src <- source
  whiteSpace
  string "-> "
  sink <- many lower
  return $ Instruction src sink

part1 :: String -> String
part1 input = handleParse instructions
  where
    instructions = mapM (parseString instruction mempty) (lines input)
    insMap is = M.fromList [(i.sink, i.source) | i <- is]
    handleParse (Success ins) = show $ evalWire (insMap ins) "a"
    handleParse (Failure e) = "Unable to parse instructions: " ++ show e

evalWire :: M.Map String Input -> String -> Word16
evalWire m key = fst $ evalWireWithCache M.empty key
  where
    evalWireWithCache :: M.Map String Word16 -> String -> (Word16, M.Map String Word16)
    evalWireWithCache cache k = case M.lookup k cache of
      Just result -> (result, cache)
      Nothing -> case M.lookup k m of
        Nothing -> error $ "Unable to find key in map: " ++ k
        Just src ->
          let (result, newCache) = evalInput cache src
              finalCache = M.insert k result newCache
           in (result, finalCache)

    evalInput :: M.Map String Word16 -> Input -> (Word16, M.Map String Word16)
    evalInput cache src = case src of
      Value v -> (v, cache)
      Wire w -> evalWireWithCache cache w
      GateRef g -> case g of
        AND i1 i2 ->
          let (v1, c1) = evalInput cache i1
              (v2, c2) = evalInput c1 i2
           in (v1 .&. v2, c2)
        OR i1 i2 ->
          let (v1, c1) = evalInput cache i1
              (v2, c2) = evalInput c1 i2
           in (v1 .|. v2, c2)
        LSHIFT i1 i2 ->
          let (v1, c1) = evalInput cache i1
              (v2, c2) = evalInput c1 i2
           in (v1 `shiftL` fromIntegral v2, c2)
        RSHIFT i1 i2 ->
          let (v1, c1) = evalInput cache i1
              (v2, c2) = evalInput c1 i2
           in (v1 `shiftR` fromIntegral v2, c2)
        NOT i1 ->
          let (v, c) = evalInput cache i1
           in (complement v, c)

-- just edit input file
part2 :: String -> String
part2 = part1
