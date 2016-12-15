module Advent2016.Day13 where

import Prelude
import Advent2016.Util (mfilter, bestFirstSearch)
import Control.Monad.Rec.Class (Step(..), tailRec)
import Control.MonadZero (guard)
import Data.Array (foldMap, intercalate, uncons, (..))
import Data.Function (on)
import Data.Int (odd)
import Data.Maybe (Maybe(..))
import Data.Ord (abs)
import Data.Set as S
import Data.Tuple (Tuple(..))

coord :: Int -> Int -> Int -> Int
coord q x y = x*x + 3*x + 2*x*y + y + y*y + q

parity :: Int -> Boolean
parity 0 = false
parity n = odd n /= parity (n / 2)

openSpace :: Int -> Int -> Int -> Boolean
openSpace q x y = not (parity (coord q x y))

data CubeFarmSt = CF { x :: Int, y :: Int, moves :: Int }

cfToPos :: CubeFarmSt -> Tuple Int Int
cfToPos (CF {x,y}) = Tuple x y

instance eqCubeFarmSt :: Eq CubeFarmSt where
    eq = eq `on` cfToPos

instance ordCubeFarmSt :: Ord CubeFarmSt where
    compare = comparing cfToPos

cfBranch :: Int -> CubeFarmSt -> Array CubeFarmSt
cfBranch plan (CF {x,y,moves}) = do
    Tuple dx dy <- [Tuple 1 0, Tuple 0 1, Tuple (-1) 0, Tuple 0 (-1)]
    let x' = x + dx
        y' = y + dy
    guard $ x' >= 0 && y' >= 0 && (dx /= 0 || dy /= 0) && openSpace plan x' y'
    pure $ CF { x: x', y : y', moves: moves + 1 }

cfGoal :: Int -> Int -> CubeFarmSt -> Boolean
cfGoal gx gy (CF {x,y}) = gx == x && gy == y

cfCost :: Int -> Int -> CubeFarmSt -> Int
cfCost gx gy (CF {x,y,moves}) = moves + abs (gx - x) + abs (gy - y)

explore :: forall a. Ord a =>
               (a -> Array a)   -- branch function
            -> a                -- initial state
            -> Int              -- visited location count
explore branch s0 = tailRec go init
    where
        init = { q:[s0], visited: S.empty }
        go {q, visited} =
            case uncons q of
                Just {head:s, tail} ->
                    if S.member s visited
                    then Loop {q:tail, visited}
                    else let new = branch s
                             newQ = tail <> new
                         in Loop {q:newQ, visited: S.insert s visited}
                Nothing -> Done $ S.size visited

showPlan :: Int -> Int -> Int -> String
showPlan plan w h = intercalate "\n" $ row <$> (0..(h-1))
    where
        row r = foldMap (\c -> if openSpace plan c r then "." else "#") (0..(w-1))

day13 :: Int -> Int -> Int -> { part1 :: Maybe Int, part2 :: Int }
day13 plan goalX goalY = { part1: getMoves <$> r, part2: r2 }
    where
        g = Tuple goalX goalY
        startPos = CF { x:1, y:1, moves:0 }
        r = bestFirstSearch (cfBranch plan)
                            (cfGoal goalX goalY)
                            (cfCost goalX goalY)
                            startPos
        r2 = explore (cfBranch plan >>> mfilter (\(CF s) -> s.moves <= 50)) startPos
        getMoves (CF {moves}) = moves
