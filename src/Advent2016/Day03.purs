module Advent2016.Day03 where

import Prelude
import Advent2016.Util (mkre, lines, count)
import Data.Array (tail)
import Data.Int (fromString)
import Data.List (List(..), transpose, concat, fromFoldable)
import Data.Maybe (Maybe)
import Data.String.Regex (match)
import Data.Traversable (traverse, sequence)

parse :: String -> Maybe (Array (Array Int))
parse = lines >>> traverse (match re >=> tail >=> sequence >=> traverse fromString)
    where
        re = mkre "(\\d+)\\s+(\\d+)\\s+(\\d+)"

chunk3 :: forall a b. (a -> a -> a -> b) -> List a -> List b
chunk3 f (Cons a (Cons b (Cons c rest))) = Cons (f a b c) (chunk3 f rest)
chunk3 _ _ = Nil

isTri :: Int -> Int -> Int -> Boolean
isTri a b c = a + b > c && b + c > a && c + a > b

countTris :: List Int -> Int
countTris = count id <<< chunk3 isTri

day03 :: Array (Array Int) -> { numTris :: Int, numTris2 :: Int }
day03 numbers =
    { numTris:  countTris (concat ns)
    , numTris2: countTris (concat (transpose ns))
    }
  where
    ns = fromFoldable <$> fromFoldable numbers
