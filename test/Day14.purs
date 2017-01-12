module Test.Day14 where

import Prelude
import Control.Monad.Eff.Console (log)
import Test.Unit (TestMain, Test, test, runTests, equal)
import Advent2016.Day14 (day14)

testDay14 :: forall e. Test e
testDay14 = test "day 14" do
    let result = day14 "zpqevtbw"
    log $ "<part1>: " <> show result.part1
    log $ "<part2>: " <> show result.part2
    equal 16106 result.part1
    equal 22423 result.part2

examples :: forall e. Test e
examples = test "day 14 examples" do
    let result = day14 "abc"
    equal 22728 result.part1
    equal 22551 result.part2

main :: forall e. TestMain e
main = runTests [examples, testDay14]
