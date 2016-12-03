module Advent2016.Day03 where

import Prelude
import Data.Array as Array
import Data.Either (either)
import Data.Int (fromString)
import Data.List (List(..), transpose, concat, fromFoldable, filter, length)
import Data.Maybe (Maybe(..))
import Data.String (split, trim, Pattern(..))
import Data.String.Regex (regex, match)
import Data.String.Regex.Flags (noFlags)
import Data.Traversable (traverse, sequence)

parse :: String -> Maybe (List (List Int))
parse s = do
    re <- either (const Nothing) Just $ regex "(\\d+)\\s+(\\d+)\\s+(\\d+)" noFlags
    s # trim >>> split (Pattern "\n")
        >>> traverse (match re >=> Array.tail >=> sequence >=> traverse fromString)
        <#> fromFoldable >>> map fromFoldable

chunk3 :: forall a b. (a -> a -> a -> b) -> List a -> List b
chunk3 f (Cons a (Cons b (Cons c rest))) = Cons (f a b c) (chunk3 f rest)
chunk3 _ _ = Nil

isTri :: Int -> Int -> Int -> Boolean
isTri a b c = a + b > c && b + c > a && c + a > b

countTris :: List Int -> Int
countTris = length <<< filter id <<< chunk3 isTri

day03 :: String -> Maybe { numTris :: Int, numTris2 :: Int }
day03 input = do
    numbers <- parse input
    pure { numTris: countTris (concat numbers)
         , numTris2: countTris (concat $ transpose numbers)
         }
