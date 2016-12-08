module Advent2016.Day06 where

import Prelude
import Data.Array ((..), (!!), length, group')
import Data.Maybe (maybe)
import Data.NonEmpty (head, tail)
import Data.String as S
import Data.String.Unsafe (charAt)
import Data.String.Utils (lines)
import Data.Traversable (minimumBy, maximumBy)
import Data.Tuple (Tuple(..), fst, snd)

mostChar :: Array (Tuple Int Char) -> Char
mostChar = maximumBy (comparing fst) >>> maybe '-' snd

leastChar :: Array (Tuple Int Char) -> Char
leastChar = minimumBy (comparing fst) >>> maybe '-' snd

charCounts :: Array String -> Array (Array (Tuple Int Char))
charCounts strs = countPos <$> (0..n)
    where
        n = maybe 0 S.length (strs !! 0) - 1
        countPos i = map (Tuple <<< length <<< tail <*> head) <<< group' $ map (charAt i) strs

day06 :: String -> { message1 :: String, message2 :: String }
day06 input =
    let acc = charCounts <<< lines $ S.trim input
    in  { message1: S.fromCharArray $ map mostChar acc
        , message2: S.fromCharArray $ map leastChar acc
        }
