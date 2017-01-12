module Test.Day02 where

import Prelude
import Control.Monad.Eff.Console (log)
import Data.Maybe (fromJust, isJust)
import Node.FS (FS)
import Node.FS.Sync (readTextFile)
import Node.Encoding (Encoding(UTF8))
import Partial.Unsafe (unsafePartial)
import Test.Unit (TestMain, Test, runTests, test, equal, assert)
import Advent2016.Day02 (day02)

testDay02 :: forall e. Test (fs :: FS | e)
testDay02 = test "day 02" do
    resultMB <- day02 <$> readTextFile UTF8 "inputs/input02.txt"
    assert "result is Just" $ isJust resultMB
    let result = unsafePartial $ fromJust resultMB
    log $ "bathroom code 1: " <> show result.code1
    log $ "bathroom code 2: " <> show result.code2
    equal "56855" result.code1
    equal "B3C27" result.code2

examples :: forall e. Test e
examples = test "day 02 examples" do
    let resultMB = day02 "ULL\nRRDDD\nLURDL\nUUUUD\n"
    assert "result is Just" $ isJust resultMB
    let result = unsafePartial $ fromJust resultMB
    equal "1985" result.code1
    equal "5DB3" result.code2

main :: forall e. TestMain (fs :: FS | e)
main = runTests [examples, testDay02]
