module Test.Day16 where

import Prelude
import Control.Monad.Eff.Console (CONSOLE, log)
import Test.Unit (TestMain, Test, runTests, test, equal)
import Advent2016.Day16 (day16)

myinput :: String
myinput = "10011111011011001"

testDay16 :: forall e. Test (console :: CONSOLE | e)
testDay16 = test "day 16" do
    let result = day16 272 myinput
    log $ "part1: " <> result
    equal "10111110010110110" result

    let result2 = day16 35651584 myinput
    log $ "part2: " <> result2
    equal "01101100001100100" result2

examples :: forall e. Test (console :: CONSOLE | e)
examples = test "day 16 examples" do
    let result = day16 20 "10000"
    equal "01100" result

main :: forall e. TestMain e
main = runTests [examples, testDay16]
