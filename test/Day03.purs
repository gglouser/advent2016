module Test.Day03 where

import Prelude
import Control.Monad.Eff.Console (log)
import Data.Maybe (Maybe(..))
import Node.FS (FS)
import Node.FS.Sync (readTextFile)
import Node.Encoding (Encoding(UTF8))
import Test.Unit (TestMain, Test, runTests, test, equal, failure)
import Advent2016.Day03 (day03, parse)

testDay03 :: forall e. Test (fs :: FS | e)
testDay03 = test "day 03" do
    inputMB <- parse <$> readTextFile UTF8 "inputs/input03.txt"
    case inputMB of
        Nothing -> failure "parse failed"
        Just input -> do
            let result = day03 input
            log $ "num triangles: " <> show result.numTris
            equal 982 result.numTris
            log $ "num triangles 2: " <> show result.numTris2
            equal 1826 result.numTris2

examples :: forall e. Test e
examples = test "day 03 examples" do
    let example = [[5, 10, 25], [3, 4, 5]]
    equal (Just example) (parse "5 10 25\n3 4 5\n")
    let result = day03 example
    equal 1 result.numTris
    equal 0 result.numTris2

main :: forall e. TestMain (fs :: FS | e)
main = runTests [examples, testDay03]
