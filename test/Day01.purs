module Test.Day01 where

import Prelude
import Control.Monad.Eff.Console (log)
import Data.Maybe (Maybe(..), fromJust, isJust)
import Node.FS (FS)
import Node.FS.Sync (readTextFile)
import Node.Encoding (Encoding(UTF8))
import Partial.Unsafe (unsafePartial)
import Test.Unit (TestMain, Test, runTests, test, equal, assert)
import Advent2016.Day01 (day01)

testDay01 :: forall e. Test (fs :: FS | e)
testDay01 = test "day 01" do
    resultMB <- day01 <$> readTextFile UTF8 "inputs/input01.txt"
    assert "result is Just" $ isJust resultMB
    let result = unsafePartial $ fromJust resultMB
    log $ "Easter Bunny HQ is " <> show result.hqDist <> " blocks away"
    log $ "*Actual* Easter Bunny HQ is " <> show result.actualHQDist <> " blocks away"
    equal 241 result.hqDist
    equal (Just 116) result.actualHQDist

examples :: forall e. Test e
examples = unsafePartial $ test "day 01 examples" do
    equal 5 (fromJust $ day01 "R2, L3").hqDist
    equal 2 (fromJust $ day01 "R2, R2, R2").hqDist
    equal 12 (fromJust $ day01 "R5, L5, R5, R3").hqDist
    equal (Just 4) (fromJust $ day01 "R8, R4, R4, R8").actualHQDist

main :: forall e. TestMain (fs :: FS | e)
main = runTests [examples, testDay01]
