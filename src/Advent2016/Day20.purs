module Advent2016.Day20 where

import Prelude
import Advent2016.Util (lines)
import Data.Array (unsafeIndex, (:), reverse, uncons, zipWith, sort)
import Data.Array.Partial (head, tail)
import Data.Maybe (Maybe(..))
import Data.String (split, Pattern(..))
import Data.Traversable (foldl, sum)
import Data.Tuple (Tuple(..), snd)
import Global (readInt)
import Partial.Unsafe (unsafePartial)

parse :: Partial => String -> Array (Tuple Number Number)
parse = lines >>> map (split (Pattern "-") >>> map (readInt 10) >>> (Tuple <$> ix 0 <*> ix 1))
    where ix i a = unsafeIndex a i

mergeBlocks :: Array (Tuple Number Number) -> Array (Tuple Number Number)
mergeBlocks bs = case uncons bs of
                    Just {head,tail} -> finish $ foldl f {cur:head, done:[]} tail
                    Nothing -> []
    where
        f {cur:Tuple a b, done} (Tuple c d)
            | c <= b+1.0 = {cur:Tuple a (max b d), done}
            | otherwise = {cur:Tuple c d, done:(Tuple a b):done}
        finish {cur,done} = reverse $ cur:done

tally :: Partial => Array (Tuple Number Number) -> Number
tally xs = sum $ zipWith f <*> tail $ xs
    where
        f (Tuple _ a) (Tuple b _) = b - a - 1.0

day20 :: Number -> String -> { part1 :: Number, part2 :: Number }
day20 maxIP input = unsafePartial $
    let bl = parse input
        merged = mergeBlocks $ sort bl
    in { part1: 1.0 + snd (head merged)
       , part2: tally $ merged <> [Tuple (maxIP+1.0) (maxIP+1.0)]
       }
