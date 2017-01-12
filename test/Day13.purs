module Test.Day13 where

import Prelude
import Control.Monad.Eff.Console (log)
import Data.Maybe (Maybe(..))
import Test.Unit (TestMain, Test, runTests, test, equal)
import Advent2016.Day13 (day13)

testDay13 :: forall e. Test e
testDay13 = test "day 13" do
    let result = day13 1362 31 39
    log $ "minimum steps to 31 39: " <> show result.part1
    log $ "num locations reachable in 50 steps: " <> show result.part2
    equal (Just 82) result.part1
    equal 138 result.part2

examples :: forall e. Test e
examples = test "day 13 examples" do
    let result = day13 10 7 4
    equal (Just 11) result.part1
    equal 151 result.part2

main :: forall e. TestMain e
main = runTests [examples, testDay13]
