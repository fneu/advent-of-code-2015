module Day21 where

import Data.List (maximumBy, minimumBy)
import Data.Ord (comparing)
import DayTypes (Day (..))

class (Show a) => Character a where
  takeHit :: Int -> a -> a
  isAlive :: a -> Bool
  damage :: a -> Int
  armor :: a -> Int

data Boss = Boss
  { hp :: Int,
    damage :: Int,
    armor :: Int
  }
  deriving (Show)

instance Character Boss where
  takeHit :: Int -> Boss -> Boss
  takeHit dmg boss = boss {hp = boss.hp - dmg}
  isAlive boss = boss.hp > 0
  damage = (.damage)
  armor = (.armor)

data Item = Item
  { cost :: Int,
    damage :: Int,
    armor :: Int
  }
  deriving (Eq, Show)

data Player = Player
  { hitpoints :: Int,
    items :: [Item]
  }
  deriving (Show)

instance Character Player where
  takeHit dmg player = player {hitpoints = player.hitpoints - dmg}
  isAlive player = player.hitpoints > 0
  damage p = sum $ map (.damage) p.items
  armor p = sum $ map (.armor) p.items

playerWins :: (Character a, Character b) => a -> b -> Bool
playerWins a b =
  let dmg = max 1 (damage a - armor b)
      b' = takeHit dmg b
   in (not (isAlive b') || not (playerWins b' a))

parseBoss :: String -> Boss
parseBoss input =
  let ls = lines input
      hp = words (head ls) !! 2
      dmg = words (ls !! 1) !! 1
      arm = words (ls !! 2) !! 1
   in Boss (read hp) (read dmg) (read arm)

weapons :: [Item]
weapons =
  [ Item 8 4 0,
    Item 10 5 0,
    Item 25 6 0,
    Item 40 7 0,
    Item 74 8 0
  ]

armors :: [Item]
armors =
  [ Item 0 0 0,
    Item 13 0 1,
    Item 31 0 2,
    Item 53 0 3,
    Item 75 0 4,
    Item 102 0 5
  ]

rings :: [[Item]]
rings =
  let rings' =
        [ Item 25 1 0,
          Item 50 2 0,
          Item 100 3 0,
          Item 20 0 1,
          Item 40 0 2,
          Item 80 0 3
        ]
   in [[Item 0 0 0]]
        <> map (: []) rings'
        <> [[x, y] | x <- rings', y <- rings', x /= y]

kitCost :: Player -> Int
kitCost player = sum $ map (.cost) player.items

day21 :: Day
day21 = Day part1 part2

part1 :: String -> String
part1 input =
  let boss = parseBoss input
      players = [Player 100 (w : a : rs) | w <- weapons, a <- armors, rs <- rings]
      winners = filter (`playerWins` boss) players
   in (show . kitCost) (minimumBy (comparing kitCost) winners)

part2 :: String -> String
part2 input =
  let boss = parseBoss input
      players = [Player 100 (w : a : rs) | w <- weapons, a <- armors, rs <- rings]
      losers = filter (\p -> not $ playerWins p boss) players
   in (show . kitCost) (maximumBy (comparing kitCost) losers)
