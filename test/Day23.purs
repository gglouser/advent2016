module Test.Day23 where

import Prelude
import Control.Monad.Eff.Console (log)
import Data.Either (Either(..))
import Node.FS (FS)
import Node.FS.Sync (readTextFile)
import Node.Encoding (Encoding(UTF8))
import Test.Unit (TestMain, Test, runTests, test, equal, failure)
import Advent2016.Day23 (day23)

testDay23 :: forall e. Test (fs :: FS | e)
testDay23 = test "day 23" do
    input <- readTextFile UTF8 "inputs/input23.txt"
    case day23 input 7 of
        Left err -> failure err
        Right result -> do
            log $ "part 1: " <> show result
            equal 11748 result

testDay23_2 :: forall e. Test (fs :: FS | e)
testDay23_2 = test "day 23 part 2" do
    input <- readTextFile UTF8 "inputs/input23.txt"
    case day23 input 12 of
        Left err -> failure err
        Right result -> do
            log $ "part 2: " <> show result
            equal 479008308 result

examples :: forall e. Test e
examples = test "day 23 examples" do
    case day23 "cpy 2 a\n\
                \tgl a\n\
                \tgl a\n\
                \tgl a\n\
                \cpy 1 a\n\
                \dec a\n\
                \dec a\n" 0 of
        Left err -> failure err
        Right result -> do
            equal 3 result

main :: forall e. TestMain (fs :: FS | e)
main = runTests [examples, testDay23, testDay23_2]
