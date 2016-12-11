module Test.Day10 where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Maybe (fromJust, isJust)
import Node.FS (FS)
import Node.FS.Sync (readTextFile)
import Node.Encoding (Encoding(UTF8))
import Partial.Unsafe (unsafePartial)
import Test.Assert (ASSERT, assert)
import Advent2016.Day10 (day10)

type Test a = forall e. Eff
                ( console :: CONSOLE
                , fs :: FS
                , err :: EXCEPTION
                , assert :: ASSERT
                | e) a

testDay10 :: Test Unit
testDay10 = unsafePartial do
    log "Running Day10"
    resultMB <- day10 17 61 <$> readTextFile UTF8 "inputs/input10.txt"
    assert $ isJust resultMB
    let result = fromJust resultMB
    log $ "bot " <> show result.part1 <> " compared values 17 and 61"
    assert $ result.part1 == 86
    log $ "product of outputs 0, 1, and 2: " <> show result.part2
    assert $ result.part2 == 22847

examples :: Test Unit
examples = unsafePartial do
    log "Running day 10 examples"
    let resultMB = day10 2 5 "value 5 goes to bot 2\n\
                         \bot 2 gives low to bot 1 and high to bot 0\n\
                         \value 3 goes to bot 1\n\
                         \bot 1 gives low to output 1 and high to bot 0\n\
                         \bot 0 gives low to output 2 and high to output 0\n\
                         \value 2 goes to bot 2\n"
    assert $ isJust resultMB
    let result = fromJust resultMB
    assert $ result.part1 == 2
    assert $ result.part2 == 30

main :: Test Unit
main = do
    examples
    testDay10
