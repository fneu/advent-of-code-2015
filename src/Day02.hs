{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}
module Day02 (day02) where

import DayTypes (Day (..))
import Text.Trifecta (Parser, Result (..), char, integer, parseString)

data Box = Box
  { a :: Integer,
    b :: Integer,
    c :: Integer
  }
  deriving (Show)

parseBox :: Parser Box
parseBox = do
  a <- integer
  _ <- char 'x'
  b <- integer
  _ <- char 'x'
  c <- integer
  return $ Box a b c

parseBoxes :: String -> [Box]
parseBoxes s = case mapM (parseString parseBox mempty) (lines s) of
  Success boxes -> boxes
  Failure _ -> error "parsing failed"

area :: Box -> Integer
area box = 2 * box.a * box.b + 2 * box.b * box.c + 2 * box.c * box.a

smallestFace :: Box -> Integer
smallestFace box = minimum [box.a * box.b, box.b * box.c, box.c * box.a]

smallestPerimeter :: Box -> Integer
smallestPerimeter box =
  minimum
    [ 2 * (box.a + box.b),
      2 * (box.b + box.c),
      2 * (box.c + box.a)
    ]

volume :: Box -> Integer
volume box = box.a * box.b * box.c

paper :: Box -> Integer
paper box = area box + smallestFace box

ribbon :: Box -> Integer
ribbon box = volume box + smallestPerimeter box

day02 :: Day
day02 = Day part1 part2

part1 :: String -> String
part1 s = show . sum $ map paper (parseBoxes s)

part2 :: String -> String
part2 s = show . sum $ map ribbon (parseBoxes s)
