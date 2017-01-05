module Test.Day25 where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Node.FS (FS)
import Node.FS.Sync (readTextFile)
import Node.Encoding (Encoding(UTF8))
import Test.Unit (runTests, test, equal)
import Advent2016.Day25 (day25)

testDay25 :: forall e. Eff (console :: CONSOLE, fs :: FS | e) Boolean
testDay25 = test "day 25" do
    input <- readTextFile UTF8 "inputs/input25.txt"
    let result = day25 input
    log $ "part 1: " <> show result.part1
    equal 196 result.part1

main :: forall e. Eff (console :: CONSOLE, fs :: FS | e) Unit
main = runTests [testDay25]
