module Advent2016.Day19 where

import Prelude
import Data.Array (drop, foldl, take, uncons, (:))
import Data.Maybe (Maybe(..))

digitsB :: Int -> Int -> Array Int
digitsB b 0 = [0]
digitsB b nn = go [] nn
    where
        go a 0 = a
        go a n = go ((n `mod` b) : a) (n / b)

undigitsB :: Int -> Array Int -> Int
undigitsB b = foldl (\a d -> a*b + d) 0

josephus :: Int -> Int
josephus = undigitsB 2 <<< (append <$> drop 1 <*> take 1) <<< digitsB 2

elfGame :: Int -> Int
elfGame n = case uncons (digitsB 3 (n-1)) of
                Just {head:1,tail} -> 1 + undigitsB 3 tail
                Just {head:2,tail} -> 2 + undigitsB 3 (1 : map (2 * _) tail)
                _ -> 0

day19 :: Int -> { part1 :: Int, part2 :: Int }
day19 input = { part1: josephus input, part2: elfGame input }
