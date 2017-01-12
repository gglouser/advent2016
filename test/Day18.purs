module Test.Day18 where

import Prelude
import Control.Monad.Eff.Console (CONSOLE, log)
import Node.FS (FS)
import Node.FS.Sync (readTextFile)
import Node.Encoding (Encoding(UTF8))
import Test.Unit (TestMain, Test, runTests, test, equal)
import Advent2016.Day18 (day18)

testDay18 :: forall e. Test (console :: CONSOLE, fs :: FS | e)
testDay18 = test "day 18" do
    input <- readTextFile UTF8 "inputs/input18.txt"
    let result = day18 40 input
    log $ "part 1: " <> show result
    equal 1982 result

    let result2 = day18 400000 input
    log $ "part 2: " <> show result2
    equal 20005203 result2

examples :: forall e. Test (console :: CONSOLE | e)
examples = test "day 18 examples" do
    let result = day18 10 ".^^.^.^^^^"
    equal 38 result

main :: forall e. TestMain (fs :: FS | e)
main = runTests [examples, testDay18]
