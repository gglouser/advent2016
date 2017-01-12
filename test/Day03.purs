module Test.Day03 where

import Prelude
import Control.Monad.Eff.Console (log)
import Data.Maybe (fromJust, isJust)
import Node.FS (FS)
import Node.FS.Sync (readTextFile)
import Node.Encoding (Encoding(UTF8))
import Partial.Unsafe (unsafePartial)
import Test.Unit (TestMain, Test, runTests, test, equal, assert)
import Advent2016.Day03 (day03)

testDay03 :: forall e. Test (fs :: FS | e)
testDay03 = test "day 03" do
    resultMB <- day03 <$> readTextFile UTF8 "inputs/input03.txt"
    assert "result is Just" $ isJust resultMB
    let result = unsafePartial $ fromJust resultMB
    log $ "num triangles: " <> show result.numTris
    log $ "num triangles 2: " <> show result.numTris2
    equal 982 result.numTris
    equal 1826 result.numTris2

examples :: forall e. Test e
examples = test "day 03 examples" do
    let resultMB = day03 "5 10 25\n3 4 5\n"
    assert "result is Just" $ isJust resultMB
    let result = unsafePartial $ fromJust resultMB
    equal 1 result.numTris
    equal 0 result.numTris2

main :: forall e. TestMain (fs :: FS | e)
main = runTests [examples, testDay03]
