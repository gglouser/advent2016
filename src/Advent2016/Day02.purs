module Advent2016.Day02 where

import Prelude
import Advent2016.Util (lines)
import Control.MonadZero (guard)
import Data.Array (scanl, foldl, (!!), mapMaybe)
import Data.Maybe (Maybe, fromMaybe)
import Data.String (toCharArray, fromCharArray)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), lookup)

type Pad = Array (Array Char)

pad1 :: Pad
pad1 = map toCharArray [ "123"
                       , "456"
                       , "789"
                       ]

pad2 :: Pad
pad2 = map toCharArray [ "##1##"
                       , "#234#"
                       , "56789"
                       , "#ABC#"
                       , "##D##"
                       ]

type Moves = Array (Array Move)

data Move = U | L | D | R
derive instance eqMove :: Eq Move
instance showMove :: Show Move where
    show U = "U"
    show D = "D"
    show L = "L"
    show R = "R"

readMove :: Char -> Maybe Move
readMove c = lookup c [Tuple 'U' U, Tuple 'D' D , Tuple 'L' L, Tuple 'R' R]

getMoves :: String -> Maybe Moves
getMoves = lines >>> traverse (toCharArray >>> traverse readMove)

move :: Move -> Tuple Int Int
move U = Tuple (-1)  0
move D = Tuple   1   0
move L = Tuple   0 (-1)
move R = Tuple   0   1

padIndex :: forall a. Array (Array a) -> Tuple Int Int -> Maybe a
padIndex pad (Tuple row col) = pad !! row >>= (_ !! col)

step :: Pad -> Tuple Int Int -> Move -> Tuple Int Int
step pad pos m = fromMaybe pos do
    let pos' = pos + move m
    c <- padIndex pad pos'
    guard $ c /= '#'
    pure pos'

execute :: Pad -> Tuple Int Int -> Moves -> String
execute pad start = scanl (foldl (step pad)) start >>> mapMaybe (padIndex pad) >>> fromCharArray

day02 :: Moves -> { code1 :: String, code2 :: String }
day02 moves = { code1: execute pad1 (Tuple 1 1) moves
              , code2: execute pad2 (Tuple 2 0) moves
              }
