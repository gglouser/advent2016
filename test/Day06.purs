module Test.Day06 where

import Prelude
import Control.Monad.Eff.Console (log)
import Node.FS (FS)
import Node.FS.Sync (readTextFile)
import Node.Encoding (Encoding(UTF8))
import Test.Unit (TestMain, Test, runTests, test, equal)
import Advent2016.Day06 (day06)

testDay06 :: forall e. Test (fs :: FS | e)
testDay06 = test "day 06" do
    result <- day06 <$> readTextFile UTF8 "inputs/input06.txt"
    log $ "message1: " <> show result.message1
    log $ "message2: " <> show result.message2
    equal "xdkzukcf" result.message1
    equal "cevsgyvd" result.message2

examples :: forall e. Test e
examples = test "day 06 examples" do
    let result = day06 "eedadn\n\
                       \drvtee\n\
                       \eandsr\n\
                       \raavrd\n\
                       \atevrs\n\
                       \tsrnev\n\
                       \sdttsa\n\
                       \rasrtv\n\
                       \nssdts\n\
                       \ntnada\n\
                       \svetve\n\
                       \tesnvt\n\
                       \vntsnd\n\
                       \vrdear\n\
                       \dvrsen\n\
                       \enarar\n"
    equal "easter" result.message1
    equal "advent" result.message2

main :: forall e. TestMain (fs :: FS | e)
main = runTests [examples, testDay06]
