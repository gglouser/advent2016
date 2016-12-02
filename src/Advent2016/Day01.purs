module Advent2016.Day01 where

import Prelude
import Data.Array (head, filter, elem, scanl, replicate, foldl)
import Data.Int (fromString)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Maybe.First (First(..))
import Data.Ord (abs)
import Data.String (uncons, split, Pattern(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), swap)

data Dir = R | L
data Move = Move Dir Int
type State = { pos :: Tuple Int Int
             , heading :: Tuple Int Int
             , trace :: Array (Tuple Int Int)
             , actualHQ :: First (Tuple Int Int)
             }

toDir :: Char -> Maybe Dir
toDir 'R' = Just R
toDir 'L' = Just L
toDir _   = Nothing

getMove :: String -> Maybe Move
getMove s = do
    {head, tail} <- uncons s
    Move <$> toDir head <*> fromString tail

turn :: Dir -> State -> State
turn d st = st { heading = swap st.heading * rot d }
    where
        rot R = Tuple 1 (-1)
        rot L = Tuple (-1) 1

walk :: Int -> State -> State
walk n st = st { pos = st.pos + st.heading * Tuple n n
               , trace = st.trace <> newTrace
               , actualHQ = st.actualHQ <> actual
               }
    where
        newTrace = steps n st
        actual = First <<< head $ filter (flip elem st.trace) newTrace

steps :: Int -> State -> Array (Tuple Int Int)
steps n {pos, heading} = scanl (+) pos $ replicate n heading

move :: Move -> State -> State
move (Move dir blocks) = turn dir >>> walk blocks

start :: State
start = { pos: Tuple 0 0
        , heading: Tuple 0 1
        , trace: [Tuple 0 0]
        , actualHQ: First Nothing
        }

travel :: String -> Maybe State
travel = split (Pattern ", ") >>> traverse getMove >=> foldl (flip move) start >>> pure

dist :: Tuple Int Int -> Int
dist (Tuple x y) = abs x + abs y

day01 :: String -> { hqDist :: Int, actualHQDist :: Int }
day01 input = { hqDist: dist result.pos
              , actualHQDist: actual
              }
    where
        result = fromMaybe start $ travel input
        actual = case result.actualHQ of
                    First (Just p) -> dist p
                    _ -> -1
