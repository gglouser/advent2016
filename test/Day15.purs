module Test.Day15 where

import Prelude
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.BigInt (fromInt, toString)
import Data.Maybe (fromJust, isJust)
import Node.FS (FS)
import Node.FS.Sync (readTextFile)
import Node.Encoding (Encoding(UTF8))
import Partial.Unsafe (unsafePartial)
import Test.Unit (TestMain, Test, runTests, test, assert, equal)
import Advent2016.Day15 (day15)

testDay15 :: forall e. Test (console :: CONSOLE, fs :: FS | e)
testDay15 = test "day 15" do
    resultMB <- day15 <$> readTextFile UTF8 "inputs/input15.txt"
    assert "result is Just" $ isJust resultMB
    let result = unsafePartial $ fromJust resultMB
    log $ "part1: " <> toString result.part1
    log $ "part2: " <> toString result.part2
    equal (fromInt 317371) result.part1
    equal (fromInt 2080951) result.part2

examples :: forall e. Test (console :: CONSOLE | e)
examples = test "day 15 examples" do
    let resultMB = day15 "Disc #1 has 5 positions; at time=0, it is at position 4.\n\
                         \Disc #2 has 2 positions; at time=0, it is at position 1.\n"
    assert "result is Just" $ isJust resultMB
    let result = unsafePartial $ fromJust resultMB
    equal (fromInt 5) result.part1
    equal (fromInt 85) result.part2

main :: forall e. TestMain (fs :: FS | e)
main = runTests [examples, testDay15]
