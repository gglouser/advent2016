module Advent2016.Day12 where

import Prelude
import Advent2016.Util (readInt)
import Control.Alt ((<|>))
import Control.Monad.State (State, execState, gets, modify)
import Control.Monad.Rec.Class (Step(..), tailRecM)
import Data.Array ((!!), some, fromFoldable)
import Data.Maybe (Maybe(..))
import Data.Either (Either, either)
import Data.String (fromCharArray)
import Text.Parsing.StringParser (runParser, ParseError)
import Text.Parsing.StringParser.Combinators (sepEndBy, option)
import Text.Parsing.StringParser.String (anyDigit, char, oneOf, string, whiteSpace)
import Partial.Unsafe (unsafeCrashWith)
-- import Debug.Trace

type Reg = Char
data Val = VReg Reg | VInt Int

data Instr
    = Cpy Val Reg
    | Inc Reg
    | Dec Reg
    | Jnz Val Int

regP = oneOf ['a','b','c','d']
intP = option id (char '-' $> negate) <*> (readInt <<< fromCharArray <$> some anyDigit)
valP = VReg <$> regP <|> VInt <$> intP
instrP = string "cpy " $> Cpy <*> valP <* whiteSpace <*> regP
    <|> string "inc " $> Inc <*> regP
    <|> string "dec " $> Dec <*> regP
    <|> string "jnz " $> Jnz <*> valP <* whiteSpace <*> intP

parse :: String -> Either ParseError (Array Instr)
parse s = runParser (fromFoldable <$> instrP `sepEndBy` whiteSpace) s

type Regs = { pc :: Int, a :: Int, b :: Int, c :: Int, d :: Int }
initRegs = { pc:0, a:0, b:0, c:0, d:0 }

exec :: Array Instr -> State Regs Unit
exec prog = tailRecM go unit
    where
        go _ = do
            pc <- gets _.pc
            case prog !! pc of
                Just i -> do handle i
                             addPC 1
                             pure $ Loop unit
                Nothing -> pure $ Done unit
        
        handle (Cpy (VReg r) dst) = read r >>= write dst
        handle (Cpy (VInt n) dst) = write dst n
        handle (Inc r) = read r >>= (_ + 1) >>> write r
        handle (Dec r) = read r >>= (_ - 1) >>> write r
        handle (Jnz (VReg r) j) = do t <- read r
                                     when (t /= 0) $ addPC (j-1)
        handle (Jnz (VInt n) j) = when (n /= 0) $ addPC (j-1)
        
        read 'a' = gets _.a
        read 'b' = gets _.b
        read 'c' = gets _.c
        read 'd' = gets _.d
        read _   = unsafeCrashWith "invalid reg read"
        
        write 'a' x = modify \regs -> regs { a = x }
        write 'b' x = modify \regs -> regs { b = x }
        write 'c' x = modify \regs -> regs { c = x }
        write 'd' x = modify \regs -> regs { d = x }
        write _   _ = unsafeCrashWith "invalid reg write"

        addPC j = modify \regs -> regs { pc = regs.pc + j }

day12 :: String -> Maybe { part1 :: Int, part2 :: Int }
day12 input = do
    prog <- either (const Nothing) Just $ parse input
    -- traceAnyM prog
    let final = execState (exec prog) initRegs
    -- traceAnyM final
    let final2 = execState (exec prog) $ initRegs { c = 1 }
    -- traceAnyM final2
    pure { part1: final.a
         , part2: final2.a
         }
