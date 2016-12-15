module Test.Day13 where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Maybe (Maybe(..))
import Partial.Unsafe (unsafePartial)
import Test.Assert (ASSERT, assert)
import Advent2016.Day13 (day13)

type Test a = forall e. Eff
                ( console :: CONSOLE
                , err :: EXCEPTION
                , assert :: ASSERT
                | e) a

testDay13 :: Test Unit
testDay13 = unsafePartial do
    log "Running Day13"
    let result = day13 1362 31 39
    log $ "minimum steps to 31 39: " <> show result.part1
    log $ "num locations reachable in 50 steps: " <> show result.part2
    assert $ result.part1 == Just 82
    assert $ result.part2 == 138

examples :: Test Unit
examples = unsafePartial do
    log "Running day 13 examples"
    let result = day13 10 7 4
    assert $ result.part1 == Just 11
    assert $ result.part2 == 151

main :: Test Unit
main = do
    examples
    testDay13
