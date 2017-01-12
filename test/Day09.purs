module Test.Day09 where

import Prelude
import Control.Monad.Eff.Console (log)
import Node.FS (FS)
import Node.FS.Sync (readTextFile)
import Node.Encoding (Encoding(UTF8))
import Test.Unit (TestMain, Test, runTests, test, equal)
import Advent2016.Day09 (day09)

testDay09 :: forall e. Test (fs :: FS | e)
testDay09 = test "day 09" do
    result <- day09 <$> readTextFile UTF8 "inputs/input09.txt"
    log $ "decompressed length   : " <> show result.part1
    log $ "decompressed length v2: " <> show result.part2
    equal 70186.0 result.part1
    equal 10915059201.0 result.part2

examples :: forall e. Test e
examples = test "day 09 examples" do
    equal 6.0 (day09 "ADVENT").part1
    equal 7.0 (day09 "A(1x5)BC").part1
    equal 9.0 (day09 "(3x3)XYZ").part1
    equal 11.0 (day09 "A(2x2)BCD(2x2)EFG").part1
    equal 6.0 (day09 "(6x1)(1x3)A").part1
    equal 18.0 (day09 "X(8x2)(3x3)ABCY").part1
    equal 9.0 (day09 "(3x3)XYZ").part2
    equal 20.0 (day09 "X(8x2)(3x3)ABCY").part2
    equal 241920.0 (day09 "(27x12)(20x12)(13x14)(7x10)(1x12)A").part2
    equal 445.0 (day09 "(25x3)(3x3)ABC(2x3)XY(5x2)PQRSTX(18x9)(3x2)TWO(5x7)SEVEN").part2

main :: forall e. TestMain (fs :: FS | e)
main = runTests [examples, testDay09]
