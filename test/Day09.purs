module Test.Day09 where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Node.FS (FS)
import Node.FS.Sync (readTextFile)
import Node.Encoding (Encoding(UTF8))
import Partial.Unsafe (unsafePartial)
import Test.Assert (ASSERT, assert)
import Advent2016.Day09 (day09)

type Test a = forall e. Eff
                ( console :: CONSOLE
                , fs :: FS
                , err :: EXCEPTION
                , assert :: ASSERT
                | e) a

testDay09 :: Test Unit
testDay09 = unsafePartial do
    log "Running Day09"
    result <- day09 <$> readTextFile UTF8 "inputs/input09.txt"
    log $ "decompressed length   : " <> show result.part1
    log $ "decompressed length v2: " <> show result.part2
    assert $ result.part1 == 70186.0
    assert $ result.part2 == 10915059201.0

examples :: Test Unit
examples = unsafePartial do
    log "Running day 09 examples"
    assert $ (day09 "ADVENT").part1 == 6.0
    assert $ (day09 "A(1x5)BC").part1 == 7.0
    assert $ (day09 "(3x3)XYZ").part1 == 9.0
    assert $ (day09 "A(2x2)BCD(2x2)EFG").part1 == 11.0
    assert $ (day09 "(6x1)(1x3)A").part1 == 6.0
    assert $ (day09 "X(8x2)(3x3)ABCY").part1 == 18.0
    assert $ (day09 "(3x3)XYZ").part2 == 9.0
    assert $ (day09 "X(8x2)(3x3)ABCY").part2 == 20.0
    assert $ (day09 "(27x12)(20x12)(13x14)(7x10)(1x12)A").part2 == 241920.0
    assert $ (day09 "(25x3)(3x3)ABC(2x3)XY(5x2)PQRSTX(18x9)(3x2)TWO(5x7)SEVEN").part2 == 445.0

main :: Test Unit
main = do
    examples
    testDay09
