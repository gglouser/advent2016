module Test.Day01 where

import Prelude
import Control.Monad.Eff.Console (log)
import Data.Maybe (Maybe(..))
import Node.FS (FS)
import Node.FS.Sync (readTextFile)
import Node.Encoding (Encoding(UTF8))
import Test.Unit (TestMain, Test, runTests, test, equal, failure)
import Advent2016.Day01 (day01, parse, Move(..), Dir(..))

testDay01 :: forall e. Test (fs :: FS | e)
testDay01 = test "day 01" do
    inputMB <- parse <$> readTextFile UTF8 "inputs/input01.txt"
    case inputMB of
        Nothing -> failure "parse failed"
        Just input -> do
            let result = day01 input
            log $ "Easter Bunny HQ is " <> show result.hqDist <> " blocks away"
            equal 241 result.hqDist
            log $ "*Actual* Easter Bunny HQ is " <> show result.actualHQDist <> " blocks away"
            equal (Just 116) result.actualHQDist

examples :: forall e. Test e
examples = test "day 01 examples" do
    equal (Just [Move R 2, Move L 3, Move R 123]) (parse "R2, L3, R123\n")
    equal 5 (day01 [Move R 2, Move L 3]).hqDist
    equal 2 (day01 [Move R 2, Move R 2, Move R 2]).hqDist
    equal 12 (day01 [Move R 5, Move L 5, Move R 5, Move R 3]).hqDist
    equal (Just 4) (day01 [Move R 8, Move R 4, Move R 4, Move R 8]).actualHQDist

main :: forall e. TestMain (fs :: FS | e)
main = runTests [examples, testDay01]
