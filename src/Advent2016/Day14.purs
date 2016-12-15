module Advent2016.Day14 where

import Prelude
import Advent2016.Util (mkre)
import Crypto.MD5 (md5)
import Control.Monad.Rec.Class (tailRec, Step(..))
import Data.Array (any, length, snoc, unsafeIndex, (!!), (..))
import Data.Lazy (Lazy, defer, force)
import Data.Maybe (fromMaybe)
import Data.String (Pattern(..), contains)
import Data.String.Regex (match)
import Partial.Unsafe (unsafePartial)

hash :: String -> Int -> String
hash salt n = md5 $ salt <> show n

hashes :: String -> Int -> Array (Lazy String)
hashes salt n = (\i -> defer \_ -> hash salt i) <$> (0 .. (n-1))

search :: Array (Lazy String) -> Int -> Array Int
search hcache num = tailRec go {n:num, i:0, acc:[]}
    where
        go {n,i,acc}
            | n == 0 || i >= length hcache = Done acc
            | otherwise = if check i
                            then Loop { n:n-1, i:i+1, acc: snoc acc i }
                            else Loop { n,     i:i+1, acc }
        key1 = mkre "(.)\\1\\1"
        hget i = force <$> hcache !! i
        check i = fromMaybe false do
            h <- hget i
            m <- match key1 h
            c <- join $ m !! 1
            let p = Pattern (c <> c <> c <> c <> c)
            pure $ any (contains p <<< fromMaybe "" <<< hget) ((i+1)..(i+1000))

stretch :: Int -> String -> String
stretch n initHash = tailRec go {i:n, h:initHash}
    where
        go {i:0, h} = Done h
        go {i,   h} = Loop { i: i-1, h: md5 h }

day14 :: String -> { part1 :: Int, part2 :: Int }
day14 input = { part1: r, part2: r2 }
    where
        hcache = hashes input 30000
        r = unsafePartial $ flip unsafeIndex 63 $ search hcache 64
        
        hcache2 = map (stretch 2016) <$> hcache
        r2 = unsafePartial $ flip unsafeIndex 63 $ search hcache2 64
