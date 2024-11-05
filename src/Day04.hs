module Day04 where

import Crypto.Hash.MD5 (hash)
import Data.ByteString.Base16 (encode)
import Data.ByteString.Char8 qualified as BS
import Data.List (findIndex)
import Data.Maybe (fromMaybe)
import DayTypes (Day (..))

md5sum :: BS.ByteString -> BS.ByteString
md5sum input = encode (hash input)

startsWithNZeroes :: Int -> BS.ByteString -> Bool
startsWithNZeroes n s = BS.take n s == BS.pack (replicate n '0')

day04 :: Day
day04 = Day part1 part2

part1 :: String -> String
part1 input =
  let prefix = BS.pack $ takeWhile (/= '\n') input
      hashes =
        [ md5sum (BS.append prefix (BS.pack (show n)))
        | (n :: Integer) <- [0 ..]
        ]
   in show $ fromMaybe 0 $ findIndex (startsWithNZeroes 5) hashes

part2 :: String -> String
part2 input =
  let prefix = BS.pack $ takeWhile (/= '\n') input
      hashes =
        [ md5sum (BS.append prefix (BS.pack (show n)))
        | (n :: Integer) <- [0 ..]
        ]
   in show $ fromMaybe 0 $ findIndex (startsWithNZeroes 6) hashes
