{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Day22 where

import Data.Either (lefts, rights)
import Data.List (minimumBy)
import Data.Ord (comparing)
import DayTypes (Day (..))

data CombatState = CombatState
  { playerHealth :: Int,
    manaSpent :: Int,
    manaAvailable :: Int,
    bossHealth :: Int,
    bossDmg :: Int,
    shieldRoundsLeft :: Int,
    poisonRoundsLeft :: Int,
    rechargeRoundsLeft :: Int,
    log :: [String]
  }
  deriving (Show)

data CombatResult = PlayerWin Int [String] | BossWin deriving (Show)

bossAttack :: CombatState -> Either CombatResult CombatState
bossAttack cs =
  if newHealth > 0
    then
      Right
        cs
          { playerHealth = newHealth,
            log = "bossAttack" : cs.log
          }
    else Left BossWin
  where
    damage = case cs.shieldRoundsLeft of
      0 -> cs.bossDmg
      _ -> max 1 (cs.bossDmg - 7)
    newHealth = cs.playerHealth - damage

useMissile :: CombatState -> Either CombatResult CombatState
useMissile cs = case (newBossHealth > 0, newManaAvailable >= 0) of
  (_, False) -> Left BossWin
  (False, True) -> Left $ PlayerWin newManaSpent ("missileUsed" : cs.log)
  (True, True) ->
    Right
      cs
        { bossHealth = newBossHealth,
          manaSpent = newManaSpent,
          manaAvailable = newManaAvailable,
          log = "missileUsed" : cs.log
        }
  where
    newBossHealth = cs.bossHealth - 4
    newManaSpent = cs.manaSpent + 53
    newManaAvailable = cs.manaAvailable - 53

useDrain :: CombatState -> Either CombatResult CombatState
useDrain cs = case (newBossHealth > 0, newManaAvailable >= 0) of
  (_, False) -> Left BossWin
  (False, True) -> Left $ PlayerWin newManaSpent ("drainUsed" : cs.log)
  (True, True) ->
    Right
      cs
        { bossHealth = newBossHealth,
          playerHealth = cs.playerHealth + 2,
          manaSpent = newManaSpent,
          manaAvailable = newManaAvailable,
          log = "drainUsed" : cs.log
        }
  where
    newBossHealth = cs.bossHealth - 2
    newManaSpent = cs.manaSpent + 73
    newManaAvailable = cs.manaAvailable - 73

useShield :: CombatState -> Either CombatResult CombatState
useShield cs =
  if newManaAvailable >= 0 && cs.shieldRoundsLeft == 0
    then
      Right
        cs
          { shieldRoundsLeft = 6,
            manaSpent = cs.manaSpent + 113,
            manaAvailable = newManaAvailable,
            log = "shieldUsed" : cs.log
          }
    else Left BossWin
  where
    newManaAvailable = cs.manaAvailable - 113

tickShield :: CombatState -> CombatState
tickShield cs =
  case cs.shieldRoundsLeft of
    0 -> cs
    _ ->
      cs
        { shieldRoundsLeft = max 0 (cs.shieldRoundsLeft - 1),
          log = "shieldTicked" : cs.log
        }

usePoison :: CombatState -> Either CombatResult CombatState
usePoison cs =
  if newManaAvailable >= 0 && cs.poisonRoundsLeft == 0
    then
      Right
        cs
          { poisonRoundsLeft = 6,
            manaSpent = cs.manaSpent + 173,
            manaAvailable = newManaAvailable,
            log = "poisonUsed" : cs.log
          }
    else Left BossWin
  where
    newManaAvailable = cs.manaAvailable - 173

tickPoison :: CombatState -> Either CombatResult CombatState
tickPoison cs =
  case cs.poisonRoundsLeft of
    0 -> Right cs
    _ ->
      if newBossHealth > 0
        then
          Right $
            cs
              { bossHealth = newBossHealth,
                poisonRoundsLeft = max 0 (cs.poisonRoundsLeft - 1),
                log = "poisonTicked" : cs.log
              }
        else Left $ PlayerWin cs.manaSpent ("poisonTicked" : cs.log)
  where
    newBossHealth = cs.bossHealth - 3

useRecharge :: CombatState -> Either CombatResult CombatState
useRecharge cs =
  if newManaAvailable >= 0 && cs.rechargeRoundsLeft == 0
    then
      Right
        cs
          { rechargeRoundsLeft = 5,
            manaSpent = cs.manaSpent + 229,
            manaAvailable = newManaAvailable,
            log = "rechargeUsed" : cs.log
          }
    else Left BossWin
  where
    newManaAvailable = cs.manaAvailable - 229

tickRecharge :: CombatState -> CombatState
tickRecharge cs =
  case cs.rechargeRoundsLeft of
    0 -> cs
    _ ->
      cs
        { manaAvailable = cs.manaAvailable + 101,
          rechargeRoundsLeft = max 0 (cs.rechargeRoundsLeft - 1),
          log = "rechargeTicked" : cs.log
        }

outcomes :: CombatState -> [CombatResult]
outcomes cs =
  case tickPoison . tickShield . tickRecharge $ cs of
    Left cr -> [cr]
    Right cs' -> case bossAttack cs' of
      Left cr -> [cr]
      Right cs'' -> case tickPoison . tickShield . tickRecharge $ cs'' of
        Left cr -> [cr]
        Right cs''' -> nextOutcomes cs'''
  where
    nextOutcomes :: CombatState -> [CombatResult]
    nextOutcomes cs_ =
      let options =
            [ useMissile cs_,
              useDrain cs_,
              useShield cs_,
              usePoison cs_,
              useRecharge cs_
            ]
       in lefts options <> concatMap outcomes (rights options)

pruned :: Int -> CombatState -> [CombatResult]
pruned spendingLimit cs =
  if cs.manaSpent > spendingLimit
    then [BossWin]
    else outcomes cs

parseInitialState :: String -> CombatState
parseInitialState input =
  let ls = lines input
      hp = read $ words (head ls) !! 2
      dmg = read $ words (ls !! 1) !! 1
   in CombatState 50 0 500 hp dmg 0 0 0 []

day22 :: Day
day22 = Day part1 part2

part1 :: String -> String
part1 input =
  let initialState = parseInitialState input
      options = rights [useMissile initialState, useDrain initialState, useShield initialState, usePoison initialState, useRecharge initialState]
      results = concatMap outcomes options
      wins = [x | x@(PlayerWin _ _) <- results]
      firstFew = take 5 wins
      (PlayerWin pruneMana _) = minimumBy (comparing (\(PlayerWin mana _) -> mana)) firstFew
      prunedResults = concatMap (pruned pruneMana) options
      prunedWins = [x | x@(PlayerWin _ _) <- prunedResults]
      best = minimumBy (comparing (\(PlayerWin mana _) -> mana)) prunedWins
   in show best

outcomes2 :: CombatState -> [CombatResult]
outcomes2 cs =
  case tickPoison . tickShield . tickRecharge $ cs of
    Left cr -> [cr]
    Right cs' -> case bossAttack cs' of
      Left cr -> [cr]
      Right cs'' -> case tickHardMode cs'' of
        Left cr -> [cr]
        Right cs''' -> case tickPoison . tickShield . tickRecharge $ cs''' of
          Left cr -> [cr]
          Right cs'''' -> nextOutcomes2 cs''''
  where
    nextOutcomes2 :: CombatState -> [CombatResult]
    nextOutcomes2 cs_ =
      let options =
            [ useMissile cs_,
              useDrain cs_,
              useShield cs_,
              usePoison cs_,
              useRecharge cs_
            ]
       in lefts options <> concatMap outcomes2 (rights options)

tickHardMode :: CombatState -> Either CombatResult CombatState
tickHardMode cs =
  if newPlayerHealth > 0
    then
      Right $
        cs
          { playerHealth = newPlayerHealth,
            log = "hardModeTicked" : cs.log
          }
    else Left BossWin
  where
    newPlayerHealth = cs.playerHealth - 1

pruned2 :: Int -> CombatState -> [CombatResult]
pruned2 spendingLimit cs =
  if cs.manaSpent > spendingLimit
    then [BossWin]
    else outcomes2 cs

parseInitialState2 :: String -> CombatState
parseInitialState2 input =
  let ls = lines input
      hp = read $ words (head ls) !! 2
      dmg = read $ words (ls !! 1) !! 1
   in CombatState 49 0 500 hp dmg 0 0 0 []

part2 :: String -> String
part2 input =
  let initialState = parseInitialState2 input
      options = rights [useMissile initialState, useDrain initialState, useShield initialState, usePoison initialState, useRecharge initialState]
      results = concatMap outcomes2 options
      wins = [x | x@(PlayerWin _ _) <- results]
      firstFew = take 5 wins
      (PlayerWin pruneMana _) = minimumBy (comparing (\(PlayerWin mana _) -> mana)) firstFew
      prunedResults = concatMap (pruned2 pruneMana) options
      prunedWins = [x | x@(PlayerWin _ _) <- prunedResults]
      best = minimumBy (comparing (\(PlayerWin mana _) -> mana)) prunedWins
   in show best
