module Advent2016.Day15 where

import Prelude
import Advent2016.Util (mkre, lines)
import Data.Array (length, uncons, (!!))
import Data.BigInt (BigInt, fromInt, fromString)
import Data.Maybe (Maybe(..))
import Data.Ord (signum)
import Data.String.Regex (match)
import Data.Traversable (foldl, sequence, traverse)
import Data.Tuple (Tuple(..), fst, snd)

type Disc = { id :: BigInt, size :: BigInt, pos :: BigInt }

parse :: String -> Maybe (Array Disc)
parse = lines >>> traverse pl
    where
        re = mkre "Disc #(\\d+) has (\\d+) positions; at time=0, it is at position (\\d+)."
        pl l = do
            m <- sequence =<< match re l
            {id:_, size:_, pos:_} <$> (fromString =<< m !! 1)
                                <*> (fromString =<< m !! 2)
                                <*> (fromString =<< m !! 3)

mod' :: forall a. (Ord a, EuclideanRing a) => a -> a -> a
mod' x y = let a = mod x y in if a < zero then a + y else a

-- Extended Euclidean Algorithm
xgcd :: forall a. (Ord a, EuclideanRing a) => a -> a -> Tuple a a
xgcd a b
    | a `mod` b == zero = Tuple zero (signum b)
    | otherwise      = case xgcd b (a `mod` b) of
                        Tuple x y -> Tuple y  (x - y*(a `div` b))

-- Chinese Remainder Theorem (2 moduli)
crt :: forall a. (Ord a, EuclideanRing a) => Tuple a a -> Tuple a a -> Tuple a a
crt (Tuple a1 n1) (Tuple a2 n2) = Tuple a' n'
    where
        g = xgcd n1 n2
        n' = n1 * n2
        a' = (a1*(snd g)*n2 + a2*(fst g)*n1) `mod'` n'

-- Chinese Remainder Theorem (multple moduli)
multiCRT :: forall a. (Ord a, EuclideanRing a) => Array (Tuple a a) -> Tuple a a
multiCRT xs = case uncons xs of
                Just {head,tail} -> foldl crt head tail
                Nothing -> Tuple zero zero

toParam :: Disc -> Tuple BigInt BigInt
toParam {id,size,pos} = Tuple ((-id-pos) `mod'` size) size

day15 :: String -> Maybe { part1 :: BigInt, part2 :: BigInt }
day15 input = do
    params <- map toParam <$> parse input
    let r1 = multiCRT params
        r2 = crt r1 $ Tuple (fromInt (-1-length params)) (fromInt 11)
    pure { part1: fst r1
         , part2: fst r2
         }
