module Advent2016.Day01 where

import Prelude
import Data.Array (head, filter, elem, scanl, replicate, foldl)
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Data.Maybe.First (First(..))
import Data.Newtype (unwrap)
import Data.Ord (abs)
import Data.String (uncons, split, trim, Pattern(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), swap, lookup)

data Dir = R | L
data Move = Move Dir Int
type State = { pos :: Tuple Int Int
             , heading :: Tuple Int Int
             , trace :: Array (Tuple Int Int)
             , actualHQ :: First (Tuple Int Int)
             }

toDir :: Char -> Maybe Dir
toDir = flip lookup [Tuple 'R' R, Tuple 'L' L]

getMove :: String -> Maybe Move
getMove s = do
    {head, tail} <- uncons s
    Move <$> toDir head <*> fromString tail

getMoves :: String -> Maybe (Array Move)
getMoves = split (Pattern ",") >>> map trim >>> traverse getMove

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

dist :: Tuple Int Int -> Int
dist (Tuple x y) = abs x + abs y

day01 :: String -> Maybe { hqDist :: Int, actualHQDist :: Maybe Int }
day01 input = do
    result <- foldl (flip move) start <$> getMoves input
    pure { hqDist: dist result.pos
         , actualHQDist: dist <$> unwrap result.actualHQ
         }
