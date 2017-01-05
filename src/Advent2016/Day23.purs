module Advent2016.Day23 where

import Prelude
import Advent2016.Util (readInt)
import Control.Alt ((<|>))
import Control.Monad.State (State, execState, gets, modify)
import Control.Monad.Rec.Class (Step(..), tailRecM)
import Data.Array (drop, fromFoldable, length, modifyAt, slice, some, take, (!!))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (fromCharArray)
import Partial.Unsafe (unsafeCrashWith)
import Text.Parsing.StringParser (runParser, ParseError, Parser)
import Text.Parsing.StringParser.Combinators (sepEndBy, option)
import Text.Parsing.StringParser.String (anyDigit, char, oneOf, string, whiteSpace)

type Reg = Char
data Val = VReg Reg | VInt Int

data Instr
    = Cpy Val Val
    | Inc Val
    | Dec Val
    | Jnz Val Val
    | Tgl Val
    | Mul Reg Val Reg Reg

toggle :: Instr -> Instr
toggle (Cpy v r) = Jnz v r
toggle (Inc r) = Dec r
toggle (Dec r) = Inc r
toggle (Jnz v r) = Cpy v r
toggle (Tgl v) = Inc v
toggle m@(Mul _ _ _ _) = m

intP :: Parser Int
intP = option id (char '-' $> negate) <*> (readInt <<< fromCharArray <$> some anyDigit)

valP :: Parser Val
valP = VReg <$> oneOf ['a','b','c','d'] <|> VInt <$> intP

instrP :: Parser Instr
instrP = string "cpy " $> Cpy <*> valP <* whiteSpace <*> valP
    <|> string "inc " $> Inc <*> valP
    <|> string "dec " $> Dec <*> valP
    <|> string "jnz " $> Jnz <*> valP <* whiteSpace <*> valP
    <|> string "tgl " $> Tgl <*> valP

parse :: String -> Either ParseError (Array Instr)
parse s = runParser (fromFoldable <$> instrP `sepEndBy` whiteSpace) s

type Mach = { pc :: Int, a :: Int, b :: Int, c :: Int, d :: Int, prog :: Array Instr }

initMach :: Array Instr -> Mach
initMach prog = { pc:0, a:0, b:0, c:0, d:0, prog:prog }

exec :: State Mach Unit
exec = tailRecM go unit
    where
        go _ = do
            pc <- gets _.pc
            prog <- gets _.prog
            case prog !! pc of
                Just i -> do handle i
                             addPC 1
                             pure $ Loop unit
                Nothing -> pure $ Done unit
        
        handle (Cpy v (VReg dst)) = getVal v >>= write dst
        handle (Inc (VReg r)) = read r >>= (_ + 1) >>> write r
        handle (Dec (VReg r)) = read r >>= (_ - 1) >>> write r
        handle (Jnz v j) = do t <- getVal v
                              j' <- getVal j
                              when (t /= 0) $ addPC (j'-1)
        handle (Tgl v) = do ia <- (+) <$> gets _.pc <*> getVal v
                            prog <- gets _.prog
                            let prog' = fromMaybe prog $ modifyAt ia toggle prog
                            modify \s -> s { prog = prog' }
        handle (Mul a b c s) = do a' <- read a
                                  b' <- getVal b
                                  c' <- read c
                                  write a $ a' + b'*c'
                                  write c 0
                                  write s 0
        handle _ = pure unit

        getVal (VReg r) = read r
        getVal (VInt n) = pure n
        
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

checkMulOp :: Array Instr -> Maybe (Array Instr)
checkMulOp [Cpy b (VReg s), Inc (VReg a), Dec (VReg s'), Jnz (VReg s'') (VInt -2),
            Dec (VReg d), Jnz (VReg d') (VInt -5)]
    | s == s' && s == s'' && d == d' = Just [Mul a b d s, nop, nop, nop, nop, nop]
checkMulOp _ = Nothing

nop :: Instr
nop = Jnz (VInt 0) (VInt 0)

optimize :: Array Instr -> Array Instr
optimize = go 0
    where
        go i prog
            | i >= length prog = prog
            | otherwise = case checkMulOp (slice i (i+6) prog) of
                            Just m -> go (i+1) $ take i prog <> m <> drop (i+6) prog
                            Nothing -> go (i+1) prog

day23 :: String -> Int -> Either String Int
day23 input initA =
    case parse input of
        Left err -> Left (show err)
        Right prog -> let mach0 = (initMach (optimize prog)) { a = initA }
                          mach = execState exec mach0
                      in Right mach.a
