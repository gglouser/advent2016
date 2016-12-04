module Advent2016.Day04 where

import Prelude
import Data.Array (length, filter, sortBy, group', take)
import Data.Char (toCharCode, fromCharCode)
import Data.Maybe (Maybe(..))
import Data.Either (either)
import Data.Int (fromString)
import Data.NonEmpty (head, tail)
import Data.String (trim, toCharArray, fromCharArray) 
import Data.String.Regex (regex, match)
import Data.String.Regex.Flags (noFlags)
import Data.String.Utils (lines, mapChars)
import Data.Traversable (traverse, sequence, sum)

type Room = { name :: String, sector :: Int, checksum :: String }

parseRoom :: Array String -> Maybe Room
parseRoom [_, name, sector, checksum] = do
    sector' <- fromString sector
    pure { name: name, sector: sector', checksum: checksum }
parseRoom _ = Nothing

parse :: String -> Maybe (Array Room)
parse s = do
    re <- either (const Nothing) Just $ regex "([a-z-]+)-(\\d+)\\[([a-z]+)\\]" noFlags
    s # trim >>> lines >>> traverse (match re >=> sequence >=> parseRoom)

getChecksum :: String -> String
getChecksum name = name # toCharArray >>> filter (_ /= '-')
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

day04 :: String -> Maybe { sectorIDSum :: Int, npoStorage :: Array Int }
day04 input = do
    rooms <- parse input
    let realRooms = filter isRealRoom rooms
    pure { sectorIDSum: sum $ map _.sector realRooms
         , npoStorage: _.sector <$> filter (\r -> decryptName r == "northpole object storage") realRooms
         }
