module Test.Day21 where

import Prelude
import Control.Monad.Eff.Console (log)
import Data.Either (Either(..))
import Node.FS (FS)
import Node.FS.Sync (readTextFile)
import Node.Encoding (Encoding(UTF8))
import Test.Unit (TestMain, Test, runTests, test, equal, failure)
import Advent2016.Day21 (day21)

testDay21 :: forall e. Test (fs :: FS | e)
testDay21 = test "day 21" do
    input <- readTextFile UTF8 "inputs/input21.txt"
    let result' = day21 input "abcdefgh" "fbgdceah"
    case result' of
        Left err -> failure err
        Right result -> do
            log $ "part 1: " <> show result.part1
            equal "baecdfgh" result.part1
            log $ "part 2: " <> show result.part2
            equal "cegdahbf" result.part2

examples :: forall e. Test e
examples = test "day 21 examples" do
    let result' = day21 "swap position 4 with position 0\n\
                       \swap letter d with letter b\n\
                       \reverse positions 0 through 4\n\
                       \rotate left 1 step\n\
                       \move position 1 to position 4\n\
                       \move position 3 to position 0\n\
                       \rotate based on position of letter b\n\
                       \rotate based on position of letter d\n"
                       "abcde"
                       "decab"
    case result' of
        Left err -> failure err
        Right result -> do
            equal "decab" result.part1
            equal "abcde" result.part2

main :: forall e. TestMain (fs :: FS | e)
main = runTests [examples, testDay21]
