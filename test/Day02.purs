module Test.Day02 where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Maybe (fromJust, isJust)
import Node.FS (FS)
import Node.FS.Sync (readTextFile)
import Node.Encoding (Encoding(UTF8))
import Partial.Unsafe (unsafePartial)
import Test.Assert (ASSERT, assert)
import Advent2016.Day02 (day02)

type Test a = forall e. Eff
                ( console :: CONSOLE
                , fs :: FS
                , err :: EXCEPTION
                , assert :: ASSERT
                | e) a

testDay02 :: Test Unit
testDay02 = unsafePartial do
    log "Running Day02"
    resultMB <- day02 <$> readTextFile UTF8 "inputs/input02.txt"
    assert $ isJust resultMB
    let result = fromJust resultMB
    log $ "bathroom code 1: " <> show result.code1
    log $ "bathroom code 2: " <> show result.code2
    assert $ result.code1 == "56855"
    assert $ result.code2 == "B3C27"

examples :: Test Unit
examples = unsafePartial do
    log "Running day 2 examples"
    let resultMB = day02 "ULL\nRRDDD\nLURDL\nUUUUD\n"
    assert $ isJust resultMB
    let result = fromJust resultMB
    assert $ result.code1 == "1985"
    assert $ result.code2 == "5DB3"

main :: Test Unit
main = do
    examples
    testDay02
