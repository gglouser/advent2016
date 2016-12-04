module Advent2016.Day02 where

import Prelude
import Control.MonadZero (guard)
import Data.Array (scanl, foldl, (!!), mapMaybe)
import Data.Maybe (Maybe, fromMaybe)
import Data.String (split, trim, toCharArray, fromCharArray, Pattern(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), lookup)

type Pad = Array (Array Char)
type Moves = Array (Array (Tuple Int Int))

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

getMove :: Char -> Maybe (Tuple Int Int)
getMove c = lookup c [ Tuple 'U' (Tuple (-1) 0), Tuple 'D' (Tuple 1 0)
                     , Tuple 'L' (Tuple 0 (-1)), Tuple 'R' (Tuple 0 1) ]

getMoves :: String -> Maybe Moves
getMoves = trim >>> split (Pattern "\n") >>> traverse (toCharArray >>> traverse getMove)

padIndex :: forall a. Array (Array a) -> Tuple Int Int -> Maybe a
padIndex pad (Tuple row col) = pad !! row >>= (_ !! col)

step :: Pad -> Tuple Int Int -> Tuple Int Int -> Tuple Int Int
step pad pos d = fromMaybe pos do
    let pos' = pos + d
    c <- padIndex pad pos'
    guard $ c /= '#'
    pure pos'

execute :: Pad -> Tuple Int Int -> Moves -> String
execute pad start = scanl (foldl (step pad)) start >>> mapMaybe (padIndex pad) >>> fromCharArray

day02 :: String -> Maybe { code1 :: String, code2 :: String }
day02 input = do
    moves <- getMoves input
    pure { code1: execute pad1 (Tuple 1 1) moves
         , code2: execute pad2 (Tuple 2 0) moves
         }
