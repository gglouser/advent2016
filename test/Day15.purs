module Test.Day15 where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.BigInt (fromInt, toString)
import Data.Maybe (fromJust, isJust)
import Node.FS (FS)
import Node.FS.Sync (readTextFile)
import Node.Encoding (Encoding(UTF8))
import Partial.Unsafe (unsafePartial)
import Test.Assert (ASSERT, assert)
import Advent2016.Day15 (day15)

type Test a = forall e. Eff
                ( console :: CONSOLE
                , fs :: FS
                , err :: EXCEPTION
                , assert :: ASSERT
                | e) a

testDay15 :: Test Unit
testDay15 = unsafePartial do
    log "Running Day15"
    resultMB <- day15 <$> readTextFile UTF8 "inputs/input15.txt"
    assert $ isJust resultMB
    let result = fromJust resultMB
    log $ "part1: " <> toString result.part1
    log $ "part2: " <> toString result.part2
    assert $ result.part1 == fromInt 317371
    assert $ result.part2 == fromInt 2080951

examples :: Test Unit
examples = unsafePartial do
    log "Running day 15 examples"
    let resultMB = day15 "Disc #1 has 5 positions; at time=0, it is at position 4.\n\
                         \Disc #2 has 2 positions; at time=0, it is at position 1.\n"
    assert $ isJust resultMB
    let result = fromJust resultMB
    assert $ result.part1 == fromInt 5
    assert $ result.part2 == fromInt 85

main :: Test Unit
main = do
    examples
    testDay15
