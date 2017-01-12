module Test.Day22 where

import Prelude
import Control.Monad.Eff.Console (log)
import Node.FS (FS)
import Node.FS.Sync (readTextFile)
import Node.Encoding (Encoding(UTF8))
import Test.Unit (TestMain, Test, runTests, test, equal)
import Advent2016.Day22 (day22, parse, pprint)

testDay22 :: forall e. Test (fs :: FS | e)
testDay22 = test "day 22" do
    input <- readTextFile UTF8 "inputs/input22.txt"
    log $ pprint $ parse input
    let result = day22 input
    log $ "part 1: " <> show result.part1
    equal 1024 result.part1
    log $ "part 2: " <> show result.part2
    equal 230 result.part2

exampleInput :: String
exampleInput = "\
\Filesystem            Size  Used  Avail  Use%\n\
\/dev/grid/node-x0-y0   10T    8T     2T   80%\n\
\/dev/grid/node-x0-y1   11T    6T     5T   54%\n\
\/dev/grid/node-x0-y2   32T   28T     4T   87%\n\
\/dev/grid/node-x1-y0    9T    7T     2T   77%\n\
\/dev/grid/node-x1-y1    8T    0T     8T    0%\n\
\/dev/grid/node-x1-y2   11T    7T     4T   63%\n\
\/dev/grid/node-x2-y0   10T    6T     4T   60%\n\
\/dev/grid/node-x2-y1    9T    8T     1T   88%\n\
\/dev/grid/node-x2-y2    9T    6T     3T   66%\n"

examples :: forall e. Test e
examples = test "day 22 examples" do
    let result = day22 exampleInput
    equal 7 result.part1
    equal 7 result.part2

main :: forall e. TestMain (fs :: FS | e)
main = runTests [examples, testDay22]
