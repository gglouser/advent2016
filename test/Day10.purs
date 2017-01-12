module Test.Day10 where

import Prelude
import Control.Monad.Eff.Console (log)
import Data.Maybe (fromJust, isJust)
import Node.FS (FS)
import Node.FS.Sync (readTextFile)
import Node.Encoding (Encoding(UTF8))
import Partial.Unsafe (unsafePartial)
import Test.Unit (TestMain, Test, runTests, test, equal, assert)
import Advent2016.Day10 (day10)

testDay10 :: forall e. Test (fs :: FS | e)
testDay10 = test "day 10" do
    resultMB <- day10 17 61 <$> readTextFile UTF8 "inputs/input10.txt"
    assert "result is Just" $ isJust resultMB
    let result = unsafePartial $ fromJust resultMB
    log $ "bot " <> show result.part1 <> " compared values 17 and 61"
    equal 86 result.part1
    log $ "product of outputs 0, 1, and 2: " <> show result.part2
    equal 22847 result.part2

examples :: forall e. Test e
examples = test "day 10 examples" do
    let resultMB = day10 2 5 "value 5 goes to bot 2\n\
                         \bot 2 gives low to bot 1 and high to bot 0\n\
                         \value 3 goes to bot 1\n\
                         \bot 1 gives low to output 1 and high to bot 0\n\
                         \bot 0 gives low to output 2 and high to output 0\n\
                         \value 2 goes to bot 2\n"
    assert "result is Just" $ isJust resultMB
    let result = unsafePartial $ fromJust resultMB
    equal 2 result.part1
    equal 30 result.part2

main :: forall e. TestMain (fs :: FS | e)
main = runTests [examples, testDay10]
