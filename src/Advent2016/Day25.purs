module Advent2016.Day25 where

import Prelude
import Control.Alt ((<|>))
import Data.Array (fromFoldable, (!!))
import Data.Char (toCharCode)
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..), maybe)
import Text.Parsing.StringParser (runParser, ParseError, Parser)
import Text.Parsing.StringParser.Combinators (choice, many1, option, sepEndBy)
import Text.Parsing.StringParser.String (anyDigit, char, string, whiteSpace)

data Reg = RA | RB | RC | RD
data Val = VReg Reg | VInt Int

data Instr
    = Cpy Val Val
    | Inc Val
    | Dec Val
    | Jnz Val Val
    | Out Val

intP :: Parser Int
intP = option id (char '-' $> negate) <*> int
    where
        int = foldl (\a b -> 10*a + b) 0 <$> many1 digit
        digit = dtoi <$> anyDigit
        dtoi d = toCharCode d - toCharCode '0'

valP :: Parser Val
valP = VReg <$> choice [RA <$ char 'a', RB <$ char 'b', RC <$ char 'c', RD <$ char 'd']
   <|> VInt <$> intP

instrP :: Parser Instr
instrP = string "cpy " $> Cpy <*> valP <* whiteSpace <*> valP
    <|> string "inc " $> Inc <*> valP
    <|> string "dec " $> Dec <*> valP
    <|> string "jnz " $> Jnz <*> valP <* whiteSpace <*> valP
    <|> string "out " $> Out <*> valP

parse :: String -> Either ParseError (Array Instr)
parse s = runParser (fromFoldable <$> instrP `sepEndBy` whiteSpace) s

-----

-- I'm going to assume that the assembunny program given as input is always
-- the same *except* for the two constants in the 2nd and 3rd instructions.
--
-- The input program does the following:
-- * Multiply the two constants together
-- * Add the result to the initial contents of the A register
-- * Output the bits of that result one at a time from LSB to MSB
--
-- So, here I extract the two constants, multiply them, and then find the
-- smallest positive integer that can be added to them that creates the
-- required bit pattern.

findA :: Int -> Int
findA key = go 2
    where
        go x | x - key > 0 = x - key
             | otherwise   = go (4*x + 2)

getConst :: Instr -> Maybe Int
getConst (Cpy (VInt x) _) = Just x
getConst _                = Nothing

day25 :: String -> { part1 :: Int }
day25 input = { part1: maybe (-1) findA key }
    where
        prog = parse input
        key = case prog of
                Left err -> Nothing
                Right prog' -> do
                    a <- getConst =<< prog' !! 1
                    b <- getConst =<< prog' !! 2
                    pure $ a * b
