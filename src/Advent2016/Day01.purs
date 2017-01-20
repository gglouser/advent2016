module Advent2016.Day01 where

import Prelude
import Data.Array (head, filter, elem, scanl, replicate, foldl, last)
import Data.Int (fromString)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Maybe.First (First(..))
import Data.Newtype (unwrap)
import Data.Ord (abs)
import Data.String (uncons, split, trim, Pattern(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), lookup)

data Dir = R | L
derive instance eqDir :: Eq Dir
instance showDir :: Show Dir where
    show R = "R"
    show L = "L"

data Move = Move Dir Int
derive instance eqMove :: Eq Move
instance showMove :: Show Move where
    show (Move d n) = show d <> show n

readMove :: String -> Maybe Move
readMove s = do
    {head, tail} <- uncons s
    Move <$> lookup head [Tuple 'R' R, Tuple 'L' L] <*> fromString tail

parse :: String -> Maybe (Array Move)
parse = trim >>> split (Pattern ", ") >>> traverse readMove

type Pos = Tuple Int Int
type State = { pos :: Pos
             , heading :: Pos
             , trace :: Array Pos
             , actualHQ :: First Pos
             }

modifyHeading :: (Pos -> Pos) -> State -> State
modifyHeading f st = st { heading = f st.heading }

rot :: Dir -> Pos -> Pos
rot R (Tuple x y) = Tuple y (-x)
rot L (Tuple x y) = Tuple (-y) x

turn :: Dir -> State -> State
turn d = modifyHeading (rot d)

walk :: Int -> State -> State
walk n st = st { pos = fromMaybe st.pos (last newTrace)
               , trace = st.trace <> newTrace
               , actualHQ = st.actualHQ <> actual
               }
    where
        newTrace = scanl (+) st.pos $ replicate n st.heading
        actual = First <<< head $ filter (flip elem st.trace) newTrace

move :: Move -> State -> State
move (Move dir blocks) = turn dir >>> walk blocks

dist :: Tuple Int Int -> Int
dist (Tuple x y) = abs x + abs y

day01 :: Array Move -> { hqDist :: Int, actualHQDist :: Maybe Int }
day01 moves = { hqDist: dist st.pos
              , actualHQDist: dist <$> unwrap st.actualHQ
              }
    where
        st = foldl (flip move) start moves
        start = { pos: Tuple 0 0
                , heading: Tuple 0 1
                , trace: [Tuple 0 0]
                , actualHQ: First Nothing
                }
