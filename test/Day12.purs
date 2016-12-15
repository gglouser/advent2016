module Test.Day12 where

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
import Advent2016.Day12 (day12)

type Test a = forall e. Eff
                ( console :: CONSOLE
                , fs :: FS
                , err :: EXCEPTION
                , assert :: ASSERT
                | e) a

testDay12 :: Test Unit
testDay12 = unsafePartial do
    log "Running Day12"
    resultMB <- day12 <$> readTextFile UTF8 "inputs/input12.txt"
    assert $ isJust resultMB
    let result = fromJust resultMB
    log $ "<part1>: " <> show result.part1
    log $ "<part2>: " <> show result.part2
    assert $ result.part1 == 318009
    assert $ result.part2 == 9227663

examples :: Test Unit
examples = unsafePartial do
    log "Running day 12 examples"
    let resultMB = day12 "cpy 41 a\n\
                        \inc a\n\
                        \inc a\n\
                        \dec a\n\
                        \jnz a 2\n\
                        \dec a\n"
    assert $ isJust resultMB
    let result = fromJust resultMB
    assert $ result.part1 == 42
    assert $ result.part2 == 42

main :: Test Unit
main = do
    examples
    testDay12
