module Advent2016.Day16 where

import Prelude
import Data.Array (reverse, foldl, snoc)
import Data.String (toCharArray, fromCharArray, null, length, take, drop)

bitneq :: Char -> Char -> Char
bitneq a b = if a == b then '0' else '1'

grow :: String -> String
grow s = s <> "0" <> (fromCharArray <<< reverse <<< map (bitneq '1') $ toCharArray s)

growTo :: Int -> String -> String
growTo n s = if length s < n then growTo n (grow s) else take n s

factor2 :: Int -> Int
factor2 = go 1
    where
        go a n | n `mod` 2 == 0 = go (2*a) (n/2)
               | otherwise      = a

chunksOf :: Int -> String -> Array String
chunksOf n = go []
    where
        go a s | null s    = a
               | otherwise = go (snoc a (take n s)) (drop n s)

parity :: String -> Char
parity = foldl bitneq '1' <<< toCharArray

checksum :: String -> String
checksum s = fromCharArray $ parity <$> chunksOf (factor2 (length s)) s

day16 :: Int -> String -> String
day16 diskSize input = checksum $ growTo diskSize input
