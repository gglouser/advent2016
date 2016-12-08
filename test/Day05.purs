module Test.Day05 where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Test.Assert (ASSERT, assert)
import Advent2016.Day05 (day05)

type Test a = forall e. Eff
                ( console :: CONSOLE
                , err :: EXCEPTION
                , assert :: ASSERT
                | e) a

testDay05 :: Test Unit
testDay05 = do
    log "Running Day05"
    let input = "wtnhxymk"
    let result = day05 input
    log $ "password: " <> show result.password
    log $ "password 2: " <> show result.password2
    assert $ result.password == "2414bc77"
    assert $ result.password2 == "437e60fc"

examples :: Test Unit
examples = do
    log "Running day 5 examples"
    let result = day05 "abc"
    assert $ result.password == "18f47a30"
    assert $ result.password2 == "05ace8e3"

main :: Test Unit
main = do
    examples
    testDay05
