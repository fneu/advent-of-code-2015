{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}
module Day15 where

import Data.List (maximumBy)
import Data.Ord (comparing)
import DayTypes (Day (..))
import Text.Trifecta (CharParsing (string), Parser, Result (Failure, Success), char, integer, letter, many, whiteSpace)
import Text.Trifecta.Parser (parseString)

data Ingredient = Ingredient
  { capacity :: Integer,
    durability :: Integer,
    flavor :: Integer,
    texture :: Integer,
    calories :: Integer
  }
  deriving (Show)

parseIngredient :: Parser Ingredient
parseIngredient = do
  _ <- many letter
  _ <- char ':'
  whiteSpace
  _ <- string "capacity "
  capa <- integer
  _ <- string ", durability "
  dura <- integer
  _ <- string ", flavor "
  flav <- integer
  _ <- string ", texture "
  text <- integer
  _ <- string ", calories "
  calo <- integer
  return $ Ingredient capa dura flav text calo

score :: [(Integer, Ingredient)] -> Integer
score pairs =
  let capacity = max 0 (sum $ [fst pair * (snd pair).capacity | pair <- pairs])
      durability = max 0 (sum $ [fst pair * (snd pair).durability | pair <- pairs])
      flavor = max 0 (sum $ [fst pair * (snd pair).flavor | pair <- pairs])
      texture = max 0 (sum $ [fst pair * (snd pair).texture | pair <- pairs])
   in if sum (map fst pairs) == 100
        then capacity * durability * flavor * texture
        else error "Sum of ingredients is not 100"

cals :: [(Integer, Ingredient)] -> Integer
cals pairs = sum $ [fst pair * (snd pair).calories | pair <- pairs]

candidates :: [Integer] -> [[Integer]]
candidates amounts =
  [ [a, b, c, 100 - (a + b + c)]
  | a <- [max 0 (head amounts - 1) .. (head amounts + 1)],
    b <- [max 0 ((amounts !! 1) - 1) .. (amounts !! 1) + 1],
    c <- [max 0 ((amounts !! 2) - 1) .. (amounts !! 2 + 1)],
    a + b + c <= 100
  ]

step :: [(Integer, Ingredient)] -> [(Integer, Ingredient)]
step pairs =
  let (amounts, ingredients) = unzip pairs
      candidatePairs = map (`zip` ingredients) (candidates amounts)
   in maximumBy (comparing score) candidatePairs

day15 :: Day
day15 = Day part1 part2

opti :: [(Integer, Ingredient)] -> Integer
opti start =
  let next = step start
      scoreBefore = score start
      scoreAfter = score next
   in if scoreAfter > scoreBefore
        then opti next
        else scoreBefore

part1 :: String -> String
part1 input = case mapM (parseString parseIngredient mempty) (lines input) of
  Failure e -> error $ "Could not parse ingredients: " ++ show e
  Success ingredients -> show . opti $ zip [10, 20, 30, 40] ingredients

--                                         ^^^^^^^^^^^^^^^^
-- manually chosen start vector that doesn't score zero for my input

part2 :: String -> String
part2 input = case mapM (parseString parseIngredient mempty) (lines input) of
  Failure e -> error $ "Could not parse ingredients: " ++ show e
  Success ingredients -> show . opti2 $ zip [30, 30, 31, 9] ingredients

--                                         ^^^^^^^^^^^^^^^^
-- manually chosen start vector again

opti2 :: [(Integer, Ingredient)] -> Integer
opti2 start =
  let next = step2 start
      scoreBefore = score start
      scoreAfter = score next
   in if scoreAfter > scoreBefore
        then opti2 next
        else scoreBefore

step2 :: [(Integer, Ingredient)] -> [(Integer, Ingredient)]
step2 pairs =
  let (amounts, ingredients) = unzip pairs
      candidatePairs = map (`zip` ingredients) (candidates amounts)
   in maximumBy (comparing (\pair -> if cals pair == 500 then score pair else 0)) candidatePairs
