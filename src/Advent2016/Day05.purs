module Advent2016.Day05 where

import Prelude
import Crypto.MD5 (md5)
import Data.Foldable (fold)
import Data.Int (fromString)
import Data.List (List(..), (:), reverse, elem, delete, (..), sort)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (take, drop)
import Data.Tuple (Tuple(..), snd)

tryIndex :: String -> Int -> Maybe String
tryIndex input n = let hash = md5 $ input <> show n
                   in if take 5 hash == "00000" then Just (take 1 (drop 5 hash)) else Nothing

search :: String -> Int -> List String
search input = go Nil 0
    where
        go a _ 0 = a
        go a n k = case tryIndex input n of
                        Just d -> go (d : a) (n+1) (k-1)
                        Nothing -> go a (n+1) k

crackPassword :: String -> String
crackPassword input = fold <<< reverse $ search input 8

tryIndex2 :: String -> Int -> Maybe (Tuple Int String)
tryIndex2 input n = let hash = md5 $ input <> show n
                    in if take 5 hash == "00000"
                       then let pos = fromMaybe (-1) <<< fromString $ take 1 (drop 5 hash)
                                c = take 1 (drop 6 hash)
                            in Just (Tuple pos c)
                       else Nothing

search2 :: String -> Int -> List (Tuple Int String)
search2 input m = go Nil 0 (0..(m-1))
    where
        go a _ Nil = a
        go a n unused = case tryIndex2 input n of
                            Just d@(Tuple pos c) ->
                                if elem pos unused
                                then go (d : a) (n+1) (delete pos unused)
                                else go a (n+1) unused
                            Nothing -> go a (n+1) unused

crackPassword2 :: String -> String
crackPassword2 input = fold <<< map snd <<< sort $ search2 input 8

day05 :: String -> { password :: String, password2 :: String }
day05 input =
    { password: crackPassword input
    , password2: crackPassword2 input
    }
