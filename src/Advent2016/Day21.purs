module Advent2016.Day21 where

import Prelude
import Control.Alt ((<|>))
import Data.Array (fromFoldable, reverse)
import Data.Char (toCharCode)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..), Replacement(..), drop, fromCharArray, indexOf,
                    length, replaceAll, singleton, take, toCharArray)
import Data.Traversable (foldl)
import Data.Tuple (Tuple(..), lookup)
import Text.Parsing.StringParser (runParser, ParseError)
import Text.Parsing.StringParser.Combinators (many1, optional, sepEndBy)
import Text.Parsing.StringParser.String (anyDigit, anyLetter, skipSpaces, string)

data Instr = SwapP Int Int
           | SwapL Char Char
           | RotL Int
           | RotR Int
           | RotC Char
           | RotUnC Char
           | Rev Int Int
           | Move Int Int

parse :: String -> Either ParseError (Array Instr)
parse = runParser instrs
    where
        instrs = fromFoldable <$> instr `sepEndBy` skipSpaces
        instr = swapp <|> swapl <|> rotl <|> rotr <|> rotc <|> rev <|> move
        swapp = string "swap position " $> SwapP <*> int <* string " with position " <*> int
        swapl = string "swap letter " $> SwapL <*> anyLetter <* string " with letter " <*> anyLetter
        rotl = string "rotate left " $> RotL <*> int <* string " step" <* optional (string "s")
        rotr = string "rotate right " $> RotR <*> int <* string " step" <* optional (string "s")
        rotc = string "rotate based on position of letter " $> RotC <*> anyLetter
        rev = string "reverse positions " $> Rev <*> int <* string " through " <*> int
        move = string "move position " $> Move <*> int <* string " to position " <*> int
        int = foldl (\a b -> 10*a + (toCharCode b - toCharCode '0')) 0 <$> many1 anyDigit

rrot :: Int -> String -> String
rrot k s = drop k' s <> take k' s
    where k' = length s - (k `mod` length s)

srev :: String -> String
srev = fromCharArray <<< reverse <<< toCharArray

cmov :: Int -> Int -> String -> String
cmov a b s = u <> c <> v
    where
        p = take a s
        c = take 1 $ drop a s
        q = drop (a+1) s
        s' = p <> q
        u = take b s'
        v = drop b s'

step :: Instr -> String -> String
step (SwapP a b) s | a < b     = s # cmov a b >>> cmov (b-1) a
                   | otherwise = s # cmov b a >>> cmov (a-1) b
step (SwapL c d) s =
    s # replaceAll (Pattern (singleton c)) (Replacement "-")
    >>> replaceAll (Pattern (singleton d)) (Replacement (singleton c))
    >>> replaceAll (Pattern "-") (Replacement (singleton d))
step (RotL n) s = drop n s <> take n s
step (RotR n) s = rrot n s
step (RotC c) s =
    case indexOf (Pattern (singleton c)) s of
        Just n | n >= 4    -> rrot (n + 2) s
               | otherwise -> rrot (n + 1) s
        Nothing -> s
step (RotUnC c) s = fromMaybe s do
    i <- indexOf (Pattern (singleton c)) s
    n <- lookup i [Tuple 0 1, Tuple 1 1, Tuple 2 6, Tuple 3 2,
                   Tuple 4 7, Tuple 5 3, Tuple 6 0, Tuple 7 4]
    pure $ drop n s <> take n s
step (Rev 0 b) s = srev (take (b+1) s) <> drop (b+1) s
step (Rev a b) s = take a s <> srev (take (b-a+1) (drop a s)) <> drop (b+1) s
step (Move a b) s = cmov a b s

undo :: Instr -> Instr
undo (RotL n) = RotR n
undo (RotR n) = RotL n
undo (RotC c) = RotUnC c
undo (RotUnC c) = RotC c
undo (Move a b) = Move b a
undo i = i

run :: Array Instr -> String -> String
run = foldl (>>>) id <<< map step

reverseProg :: Array Instr -> Array Instr
reverseProg = reverse <<< map undo

day21 :: String -> String -> String -> Either String { part1 :: String, part2 :: String }
day21 instrs plain pword =
    case parse instrs of
        Left err -> Left $ show err
        Right prog -> Right
                        { part1: run prog plain
                        , part2: run (reverseProg prog) pword
                        }
