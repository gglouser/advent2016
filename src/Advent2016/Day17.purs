module Advent2016.Day17 where

import Prelude
import Advent2016.Util (bestFirstSearch)
import Control.MonadZero ((<|>), guard)
import Crypto.MD5 (md5)
import Data.Array (uncons)
import Data.Maybe (Maybe(..))
import Data.Ord (abs)
import Data.String (length)
import Data.String.Unsafe (charAt)
import Data.Tuple (Tuple(..), fst, snd)

openDoors :: String -> { up :: Boolean, down :: Boolean, left :: Boolean, right :: Boolean }
openDoors path = { up: test 0, down: test 1, left: test 2, right: test 3 }
    where
        hash = md5 path
        test i = (between 'b' 'f') $ charAt i hash

data State = State
    { path :: String
    , pos :: Tuple Int Int
    }

instance eqState :: Eq State where
    eq (State s1) (State s2) = s1.path == s2.path

instance ordState :: Ord State where
    compare (State s1) (State s2) = comparing _.path s1 s2

instance showState :: Show State where
    show (State s) = s.path

getPath :: State -> String
getPath (State s) = s.path

branch :: String -> State -> Array State
branch passcode (State s) = do
    let doors = openDoors $ passcode <> s.path
    Tuple m d <- (guard doors.up    $> Tuple "U" (Tuple 0 (-1)))
             <|> (guard doors.down  $> Tuple "D" (Tuple 0 1))
             <|> (guard doors.left  $> Tuple "L" (Tuple (-1) 0))
             <|> (guard doors.right $> Tuple "R" (Tuple 1 0))
    let pos = s.pos + d
    guard $ between 0 3 (fst pos) && between 0 3 (snd pos)
    pure $ State { path: s.path <> m, pos: pos }

goalTest :: State -> Boolean
goalTest (State s) = s.pos == Tuple 3 3

costH :: State -> Int
costH (State s) = length s.path + abs (3 - fst s.pos) + abs (3 - snd s.pos)

longestPath :: String -> Int
longestPath passcode = go (-1) [s0]
    where
        s0 = State { path: "", pos: Tuple 0 0 }
        go best q =
            case uncons q of
                Just {head,tail} ->
                    if goalTest head
                    then go (max best (length (getPath head))) tail
                    else go best $ (branch passcode head) <> tail
                Nothing -> best

day17 :: String -> { part1 :: Maybe String }
day17 passcode = { part1: getPath <$> r1 }
    where
        s0 = State { path: "", pos: Tuple 0 0 }
        r1 = bestFirstSearch (branch passcode) goalTest costH s0

day17part2 :: String -> Int
day17part2 = longestPath
