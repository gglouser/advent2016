module Test.Day19 where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Test.Unit (runTests, test, equal)
import Advent2016.Day19 (day19)

myinput :: Int
myinput = 3005290

testDay19 :: forall e. Eff (console :: CONSOLE | e) Boolean
testDay19 = test "day 19" do
    let result = day19 myinput
    log $ "part 1: " <> show result.part1
    equal 1816277 result.part1
    log $ "part 2: " <> show result.part2
    equal 1410967 result.part2

examples :: forall e. Eff (console :: CONSOLE | e) Boolean
examples = test "day 19 examples" do
    let result = day19 5
    equal 3 result.part1
    equal 2 result.part2

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = runTests [examples, testDay19]
