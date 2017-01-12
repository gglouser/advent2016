module Test.Day08 where

import Prelude
import Control.Monad.Eff.Console (log)
import Data.Maybe (fromJust, isJust)
import Node.FS (FS)
import Node.FS.Sync (readTextFile)
import Node.Encoding (Encoding(UTF8))
import Partial.Unsafe (unsafePartial)
import Test.Unit (TestMain, Test, runTests, test, equal, assert)
import Advent2016.Day08 (day08)

testDay08 :: forall e. Test (fs :: FS | e)
testDay08 = test "day 08" do
    resultMB <- day08 50 6 <$> readTextFile UTF8 "inputs/input08.txt"
    assert "result is Just" $ isJust resultMB
    let result = unsafePartial $ fromJust resultMB
    log $ "num on: " <> show result.numOn
    equal 123 result.numOn
    log $ "final screen: "
    log result.finalScreen

examples :: forall e. Test e
examples = test "day 08 examples" do
    let resultMB = day08 7 3
                    "rect 3x2\n\
                    \rotate column x=1 by 1\n\
                    \rotate row y=0 by 4\n\
                    \rotate column x=1 by 1\n"
    assert "result is Just" $ isJust resultMB
    let result = unsafePartial $ fromJust resultMB
    equal 6 result.numOn
    equal ".#..#.#\n#.#....\n.#.....\n" result.finalScreen

main :: forall e. TestMain (fs :: FS | e)
main = runTests [examples, testDay08]
