module Day12 where

import Control.Lens (cosmos, (^..))
import Data.Aeson (Value (..), decode)
import Data.Aeson.Lens (_Integer, _String)
import Data.ByteString.Lazy.Char8 qualified as BS
import Data.Scientific qualified as Scientific
import Data.Vector qualified as V
import DayTypes (Day (..))

day12 :: Day
day12 = Day part1 part2

parseJson :: String -> Maybe Value
parseJson jsonString = decode (BS.pack jsonString)

part1 :: String -> String
part1 input = case parseJson input of
  Nothing -> error "could not parse json"
  Just value -> show . sum $ value ^.. cosmos . _Integer

sumIgnoringRed :: Value -> Scientific.Scientific
sumIgnoringRed (Number n) = n
sumIgnoringRed (Array arr) = sum (map sumIgnoringRed (V.toList arr))
sumIgnoringRed (Object obj)
  | "red" `elem` (obj ^.. traverse . _String) = 0
  | otherwise = sum (map sumIgnoringRed (obj ^.. traverse))
sumIgnoringRed _ = 0

part2 :: String -> String
part2 input = case parseJson input of
  Nothing -> error "could not parse json"
  Just value -> show $ sumIgnoringRed value
