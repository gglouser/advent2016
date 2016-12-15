module Test.Day14 where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Test.Assert (ASSERT, assert)
import Advent2016.Day14 (day14)

type Test a = forall e. Eff
                ( console :: CONSOLE
                , assert :: ASSERT
                | e) a

testDay14 :: Test Unit
testDay14 = do
    log "Running Day14"
    let result = day14 "zpqevtbw"
    log $ "<part1>: " <> show result.part1
    log $ "<part2>: " <> show result.part2
    assert $ result.part1 == 16106
    assert $ result.part2 == 22423

examples :: Test Unit
examples = do
    log "Running day 14 examples"
    let result = day14 "abc"
    assert $ result.part1 == 22728
    assert $ result.part2 == 22551

main :: Test Unit
main = do
    examples
    testDay14
