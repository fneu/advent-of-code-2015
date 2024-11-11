module Day16 where

import Control.Applicative ((<|>))
import Data.Maybe (mapMaybe)
import DayTypes (Day (..))
import Text.Trifecta (CharParsing (string), Parser, Result (Failure, Success), integer)
import Text.Trifecta.Parser (parseString)

day16 :: Day
day16 = Day part1 part2

matching :: Parser String
matching =
  string "children: 3"
    <|> string "cats: 7"
    <|> string "samoyeds: 2"
    <|> string "pomeranians: 3"
    <|> string "akitas: 0"
    <|> string "vizslas: 0"
    <|> string "goldfish: 5"
    <|> string "trees: 3"
    <|> string "cars: 2"
    <|> string "perfumes: 1"

sue :: Parser Integer
sue = do
  _ <- string "Sue "
  value <- integer
  _ <- string ": "
  _ <- matching
  _ <- string ", "
  _ <- matching
  _ <- string ", "
  _ <- matching
  return value

part1 :: String -> String
part1 input = show $ mapMaybe toMaybe parseResults
  where
    parseResults = map (parseString sue mempty) (lines input)
    toMaybe (Success x) = Just x
    toMaybe (Failure _) = Nothing

lessThan :: Integer -> Parser Integer
lessThan n = do
  i <- integer
  if i < n
    then return i
    else fail "too big"

greaterThan :: Integer -> Parser Integer
greaterThan n = do
  i <- integer
  if i > n
    then return i
    else fail "too small"

matching2 :: Parser String
matching2 =
  string "children: 3"
    <|> string "cats: " <* greaterThan 7
    <|> string "samoyeds: 2"
    <|> string "pomeranians: " <* lessThan 3
    <|> string "akitas: 0"
    <|> string "vizslas: 0"
    <|> string "goldfish: " <* lessThan 5
    <|> string "trees: " <* greaterThan 3
    <|> string "cars: 2"
    <|> string "perfumes: 1"

sue2 :: Parser Integer
sue2 = do
  _ <- string "Sue "
  value <- integer
  _ <- string ": "
  _ <- matching2
  _ <- string ", "
  _ <- matching2
  _ <- string ", "
  _ <- matching2
  return value

part2 :: String -> String
part2 input = show $ mapMaybe toMaybe parseResults
  where
    parseResults = map (parseString sue2 mempty) (lines input)
    toMaybe (Success x) = Just x
    toMaybe (Failure _) = Nothing
