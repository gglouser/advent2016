module Test.Day12 where

import Prelude
import Control.Monad.Eff.Console (log)
import Data.Maybe (fromJust, isJust)
import Node.FS (FS)
import Node.FS.Sync (readTextFile)
import Node.Encoding (Encoding(UTF8))
import Partial.Unsafe (unsafePartial)
import Test.Unit (TestMain, Test, runTests, test, equal, assert)
import Advent2016.Day12 (day12)

testDay12 :: forall e. Test (fs :: FS | e)
testDay12 = test "day 12" do
    resultMB <- day12 <$> readTextFile UTF8 "inputs/input12.txt"
    assert "result is Just" $ isJust resultMB
    let result = unsafePartial $ fromJust resultMB
    log $ "<part1>: " <> show result.part1
    log $ "<part2>: " <> show result.part2
    equal 318009 result.part1
    equal 9227663 result.part2

examples :: forall e. Test e
examples = test "day 12 examples" do
    let resultMB = day12 "cpy 41 a\n\
                         \inc a\n\
                         \inc a\n\
                         \dec a\n\
                         \jnz a 2\n\
                         \dec a\n"
    assert "result is Just" $ isJust resultMB
    let result = unsafePartial $ fromJust resultMB
    equal 42 result.part1
    equal 42 result.part2

main :: forall e. TestMain (fs :: FS | e)
main = runTests [examples, testDay12]
