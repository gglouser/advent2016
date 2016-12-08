module Test.Day06 where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Node.FS (FS)
import Node.FS.Sync (readTextFile)
import Node.Encoding (Encoding(UTF8))
import Partial.Unsafe (unsafePartial)
import Test.Assert (ASSERT, assert)
import Advent2016.Day06 (day06)

type Test a = forall e. Eff
                ( console :: CONSOLE
                , fs :: FS
                , err :: EXCEPTION
                , assert :: ASSERT
                | e) a

testDay06 :: Test Unit
testDay06 = unsafePartial do
    log "Running Day06"
    result <- day06 <$> readTextFile UTF8 "inputs/input06.txt"
    log $ "message1: " <> show result.message1
    log $ "message2: " <> show result.message2
    assert $ result.message1 == "xdkzukcf"
    assert $ result.message2 == "cevsgyvd"

examples :: Test Unit
examples = unsafePartial do
    log "Running day 06 examples"
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
    assert $ result.message1 == "easter"
    assert $ result.message2 == "advent"

main :: Test Unit
main = do
    examples
    testDay06
