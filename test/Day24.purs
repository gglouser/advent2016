module Test.Day24 where

import Prelude
import Control.Monad.Eff.Console (CONSOLE, log)
import Node.FS (FS)
import Node.FS.Sync (readTextFile)
import Node.Encoding (Encoding(UTF8))
import Test.Unit (TestMain, Test, runTests, test, equal)
import Advent2016.Day24 (day24)

testDay24 :: forall e. Test (console :: CONSOLE, fs :: FS | e)
testDay24 = test "day 24" do
    input <- readTextFile UTF8 "inputs/input24.txt"
    let result = day24 input
    log $ "part 1: " <> show result.part1
    equal 464 result.part1
    log $ "part 2: " <> show result.part2
    equal 652 result.part2

example_input :: String
example_input =
    "###########\n\
    \#0.1.....2#\n\
    \#.#######.#\n\
    \#4.......3#\n\
    \###########\n"

examples :: forall e. Test (console :: CONSOLE | e)
examples = test "day 24 examples" do
    let result = day24 example_input
    equal 14 result.part1
    equal 20 result.part2

main :: forall e. TestMain (fs :: FS | e)
main = runTests [examples, testDay24]
