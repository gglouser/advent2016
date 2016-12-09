module Advent2016.Day09 where

import Prelude
import Control.Alt ((<|>))
import Data.Array (some, replicate)
import Data.Either (either)
import Data.Int (floor)
import Data.String (fromCharArray, trim)
import Data.Traversable (sequence, sum)
import Global (readInt)
import Text.Parsing.StringParser (Parser, runParser)
import Text.Parsing.StringParser.Combinators (many)
import Text.Parsing.StringParser.String (anyDigit, anyChar, char)

-- 32-bit Ints are not big enough for part 2.
-- Numbers do have enough precision, despite being floating point.

data Marker = M Number Number

num :: Parser Number
num = readInt 10 <<< fromCharArray <$> some anyDigit

marker :: Parser Marker
marker = char '(' $> M <*> num <* char 'x' <*> num <* char ')'

calcSize :: Boolean -> String -> Number
calcSize v2 = either (const 0.0) id <<< runParser count
    where
        count = sum <$> many (uncomp <|> (1.0 <$ anyChar))
        uncomp = do M m n <- marker
                    sub <- sequence (replicate (floor m) anyChar)
                    pure $ if v2
                            then n * calcSize true (fromCharArray sub)
                            else n * m

day09 :: String -> { part1 :: Number, part2 :: Number }
day09 input =
    { part1: calcSize false inp
    , part2: calcSize true inp
    }
    where inp = trim input
