module Test.Day03 where

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
import Advent2016.Day03 (day03)

type Test a = forall e. Eff
                ( console :: CONSOLE
                , fs :: FS
                , err :: EXCEPTION
                , assert :: ASSERT
                | e) a

testDay03 :: Test Unit
testDay03 = unsafePartial do
    log "Running Day03"
    resultMB <- day03 <$> readTextFile UTF8 "inputs/input03.txt"
    assert $ isJust resultMB
    let result = fromJust resultMB
    log $ "num triangles: " <> show result.numTris
    log $ "num triangles 2: " <> show result.numTris2
    assert $ result.numTris == 982
    assert $ result.numTris2 == 1826

examples :: Test Unit
examples = unsafePartial do
    log "Running day 3 examples"
    let resultMB = day03 "5 10 25\n3 4 5\n"
    assert $ isJust resultMB
    let result = fromJust resultMB
    assert $ result.numTris == 1
    assert $ result.numTris2 == 0

main :: Test Unit
main = do
    examples
    testDay03
