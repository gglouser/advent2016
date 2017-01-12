module Test.Day20 where

import Prelude
import Control.Monad.Eff.Console (CONSOLE, log)
import Node.FS (FS)
import Node.FS.Sync (readTextFile)
import Node.Encoding (Encoding(UTF8))
import Test.Unit (TestMain, Test, runTests, test, equal)
import Advent2016.Day20 (day20)

testDay20 :: forall e. Test (console :: CONSOLE, fs :: FS | e)
testDay20 = test "day 20" do
    input <- readTextFile UTF8 "inputs/input20.txt"
    let result = day20 4294967295.0 input
    log $ "part 1: " <> show result.part1
    equal 19449262.0 result.part1
    log $ "part 2: " <> show result.part2
    equal 119.0 result.part2

examples :: forall e. Test (console :: CONSOLE | e)
examples = test "day 20 examples" do
    let result = day20 9.0 "5-8\n0-2\n4-7\n"
    equal 3.0 result.part1
    equal 2.0 result.part2

main :: forall e. TestMain (fs :: FS | e)
main = runTests [examples, testDay20]
