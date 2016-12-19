module Advent2016.Day18 where

import Prelude
import Advent2016.Util (count)
import Data.Array (length, (!!), (..))
import Data.Maybe (fromMaybe)
import Data.String (trim, toCharArray, fromCharArray)

step :: Array Boolean -> Array Boolean
step xs = map rule (0..(length xs-1))
    where
        ix i = fromMaybe false $ xs !! i
        rule i = ix (i-1) /= ix (i+1)
            
parse :: String -> Array Boolean
parse = trim >>> toCharArray >>> map (_ == '^')

showRow :: Array Boolean -> String
showRow = fromCharArray <<< map (if _ then '^' else '.')

tally :: Int -> Array Boolean -> Int
tally nrows = go 0 nrows
    where
        go a 0 _  = a
        go a n xs = go (a + count not xs) (n-1) (step xs)

day18 :: Int -> String -> Int
day18 n input = tally n $ parse input
