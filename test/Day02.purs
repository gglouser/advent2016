module Test.Day02 where

import Prelude
import Control.Monad.Eff.Console (log)
import Data.Maybe (Maybe(..))
import Node.FS (FS)
import Node.FS.Sync (readTextFile)
import Node.Encoding (Encoding(UTF8))
import Test.Unit (TestMain, Test, runTests, test, equal, failure)
import Advent2016.Day02 (day02, getMoves, Move(..))

testDay02 :: forall e. Test (fs :: FS | e)
testDay02 = test "day 02" do
    inputMB <- getMoves <$> readTextFile UTF8 "inputs/input02.txt"
    case inputMB of
        Nothing -> failure "failed to parse"
        Just input -> do
            let result = day02 input
            log $ "bathroom code 1: " <> show result.code1
            equal "56855" result.code1
            log $ "bathroom code 2: " <> show result.code2
            equal "B3C27" result.code2

examples :: forall e. Test e
examples = test "day 02 examples" do
    let example = [[U,L,L], [R,R,D,D,D], [L,U,R,D,L], [U,U,U,U,D]]
    equal (Just example) (getMoves "ULL\nRRDDD\nLURDL\nUUUUD\n")
    let result = day02 example
    equal "1985" result.code1
    equal "5DB3" result.code2

main :: forall e. TestMain (fs :: FS | e)
main = runTests [examples, testDay02]
