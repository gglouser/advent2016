module Advent2016.Day22 where

import Prelude
import Advent2016.Util (bestFirstSearch, lines, mkre)
import Control.MonadZero (guard)
import Data.Array (filter, last, length, mapMaybe, unsafeIndex, (!!), (..))
import Data.Int as Int
import Data.Maybe (maybe)
import Data.Ord (abs)
import Data.String.Regex (match)
import Data.Traversable (find, foldMap, intercalate, notElem, sequence, sum)
import Data.Tuple (Tuple(..), fst, snd)
import Partial.Unsafe (unsafePartial)

type Pos = Tuple Int Int
data Node = Node Pos Int Int

instance showNode :: Show Node where
    show (Node (Tuple x y) u a) = "node-x" <> show x <> "-y" <> show y
                                    <> " " <> show u <> " " <> show a

nodePos :: Node -> Pos
nodePos (Node p _ _) = p

nodeUsed :: Node -> Int
nodeUsed (Node _ u _) = u

parse :: String -> Array Node
parse = mapMaybe line <<< lines
    where
        re = mkre "/dev/grid/node-x(\\d+)-y(\\d+)\\s+(\\d+)T\\s+(\\d+)T\\s+(\\d+)T\\s+(\\d+)%"
        ixInt m i = Int.fromString =<< m !! i
        line l = do m <- sequence =<< match re l
                    Node <$> (Tuple <$> ixInt m 1 <*> ixInt m 2) <*> ixInt m 4 <*> ixInt m 5

viable :: Node -> Node -> Boolean
viable (Node _ aU _) (Node _ _ bA) = aU /= 0 && aU <= bA

data State = St Pos Pos Int

instance eqState :: Eq State where
    eq (St o1 g1 _) (St o2 g2 _) = o1 == o2 && g1 == g2

instance ordState :: Ord State where
    compare (St o1 g1 _) (St o2 g2 _) = compare o1 o2 <> compare g1 g2

getMoves :: State -> Int
getMoves (St _ _ m) = m

branch :: Pos -> Array Pos -> State -> Array State
branch (Tuple xMax yMax) walls (St oldPos@(Tuple ox oy) goalDataPos moves) = do
    newPos@(Tuple nx ny) <- [Tuple (ox+1) oy, Tuple (ox-1) oy, Tuple ox (oy+1), Tuple ox (oy-1)]
    guard $ nx >= 0 && nx <= xMax && ny >= 0 && ny <= yMax && notElem newPos walls
    let goal' = if newPos == goalDataPos then oldPos else goalDataPos
    pure $ St newPos goal' (moves+1)

goalTest :: State -> Boolean
goalTest (St (Tuple ox oy) (Tuple gx gy) _) = ox == gx - 1 && oy == gy

costH :: State -> Int
costH (St (Tuple ox oy) (Tuple gx gy) moves) = moves + abs (ox - gx + 1) + abs (oy - gy)

pprint :: Array Node -> String
pprint nodes = intercalate "\n" $ foldMap nodec <<< row <$> 0 .. (snd bounds)
    where
        bounds = maybe (Tuple 0 0) nodePos $ last nodes
        row y = mapMaybe (\x -> nodes !! (y + x*(snd bounds + 1))) (0..(fst bounds))
        nodec (Node p u a)
            | p == Tuple 0 0 = "(.)"
            | p == Tuple (fst bounds) 0 = " G "
            | u == 0 = " _ "
            | u > 100 = " # "
            | otherwise = " . "

day22 :: String -> { part1 :: Int, part2 :: Int }
day22 input = { part1: sum vcount
              , part2: maybe (-1) getMoves best + 1 + 5*(fst bounds - 1)
              }
    where
        xs = parse input
        vtest p q = if viable p q then 1 else 0
        vcount = unsafePartial do
            i <- 0 .. (length xs - 2)
            let a = unsafeIndex xs i
            j <- (i+1) .. (length xs - 1)
            let b = unsafeIndex xs j
            pure $ vtest a b + vtest b a
        walls = nodePos <$> filter (\n -> nodeUsed n > 100) xs
        openPos = maybe (Tuple 0 0) nodePos $ find (\n -> nodeUsed n == 0) xs
        bounds = maybe (Tuple 0 0) nodePos $ last xs
        goalData = Tuple (fst bounds) 0
        s0 = St openPos goalData 0
        branch' = branch bounds walls
        best = bestFirstSearch branch' goalTest costH s0
