module Advent2016.Day04 where

import Prelude
import Advent2016.Util (lines, mkre)
import Data.Array (length, filter, sortBy, group', take, (!!))
import Data.Char (toCharCode, fromCharCode)
import Data.Maybe (Maybe)
import Data.Int (fromString)
import Data.NonEmpty (head, tail)
import Data.String (toCharArray, fromCharArray)
import Data.String.Regex (match)
import Data.String.Utils (mapChars)
import Data.Traversable (traverse, sequence, sum)

type Room = { name :: String, sector :: Int, checksum :: String }

parse :: String -> Maybe (Array Room)
parse = lines >>> traverse (match re >=> sequence >=> parseRoom)
    where
        re = mkre "([a-z-]+)-(\\d+)\\[([a-z]+)\\]"
        parseRoom m = {name:_,sector:_,checksum:_} <$> m !! 1
                                                   <*> (fromString =<< m !! 2)
                                                   <*> m !! 3

getChecksum :: String -> String
getChecksum = toCharArray >>> filter (_ /= '-')
            >>> group' >>> sortBy cmp
            >>> take 5 >>> map head >>> fromCharArray
    where
        cmp = comparing (negate <<< length <<< tail) <> comparing head

isRealRoom :: Room -> Boolean
isRealRoom r = getChecksum r.name == r.checksum

decryptName :: Room -> String
decryptName r = mapChars dec r.name
    where
        dec c | isLower c = fromCharCode $ (toCharCode c - ccA + r.sector) `mod` 26 + ccA
              | otherwise = ' '
        isLower c = c >= 'a' && c <= 'z'
        ccA = toCharCode 'a'

day04 :: Array Room -> { sectorIDSum :: Int, npoStorage :: Array Int }
day04 rooms = { sectorIDSum: sum (_.sector <$> realRooms)
              , npoStorage: _.sector <$> filter isNPOStorage realRooms
              }
    where
        realRooms = filter isRealRoom rooms
        isNPOStorage r = decryptName r == "northpole object storage"
