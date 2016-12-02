module Test.Day01 where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Maybe (Maybe(..), fromJust, isJust)
import Node.FS (FS)
import Node.FS.Sync (readTextFile)
import Node.Encoding (Encoding(UTF8))
import Partial.Unsafe (unsafePartial)
import Test.Assert (ASSERT, assert)
import Advent2016.Day01 (day01)

type Test a = forall e. Eff
                ( console :: CONSOLE
                , fs :: FS
                , err :: EXCEPTION
                , assert :: ASSERT
                | e) a

testDay01 :: Test Unit
testDay01 = unsafePartial do
    log "Running Day01"
    resultMB <- day01 <$> readTextFile UTF8 "inputs/input01.txt"
    assert $ isJust resultMB
    let result = fromJust resultMB
    log $ "Easter Bunny HQ is " <> show result.hqDist <> " blocks away"
    log $ "*Actual* Easter Bunny HQ is " <> show result.actualHQDist <> " blocks away"
    assert $ result.hqDist == 241
    assert $ result.actualHQDist == Just 116

examples :: Test Unit
examples = unsafePartial do
    assert $ (fromJust $ day01 "R2, L3").hqDist == 5
    assert $ (fromJust $ day01 "R2, R2, R2").hqDist == 2
    assert $ (fromJust $ day01 "R5, L5, R5, R3").hqDist == 12
    assert $ (fromJust $ day01 "R8, R4, R4, R8").actualHQDist == Just 4

main :: Test Unit
main = do
    examples
    testDay01
