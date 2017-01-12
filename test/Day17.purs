module Test.Day17 where

import Prelude
import Control.Monad.Eff.Console (log)
import Data.Maybe (Maybe(..))
import Test.Unit (TestMain, Test, runTests, test, equal)
import Advent2016.Day17 (day17, day17part2)

myinput :: String
myinput = "bwnlcvfs"

testDay17 :: forall e. Test e
testDay17 = test "day 17" do
    let result = day17 myinput
    log $ "part 1: " <> show result.part1
    equal (Just "DDURRLRRDD") result.part1

    let result2 = day17part2 myinput
    log $ "part 2: " <> show result2
    equal 436 result2

examples :: forall e. Test e
examples = test "day 17 examples" do
    let result1 = day17 "ihgpwlah"
    equal (Just "DDRRRD") result1.part1

    let result2 = day17 "kglvqrro"
    equal (Just "DDUDRLRRUDRD") result2.part1

    let result3 = day17 "ulqzkmiv"
    equal (Just "DRURDRUDDLLDLUURRDULRLDUUDDDRR") result3.part1

examples2 :: forall e. Test e
examples2 = test "day 17 part 2 examples" do
    let result1 = day17part2 "ihgpwlah"
    equal 370 result1

    let result2 = day17part2 "kglvqrro"
    equal 492 result2

    let result3 = day17part2 "ulqzkmiv"
    equal 830 result3

main :: forall e. TestMain e
main = runTests [examples, examples2, testDay17]
