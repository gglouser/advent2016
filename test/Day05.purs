module Test.Day05 where

import Prelude
import Control.Monad.Eff.Console (log)
import Test.Unit (TestMain, Test, runTests, test, equal)
import Advent2016.Day05 (day05)

testDay05 :: forall e. Test e
testDay05 = test "day 05" do
    let input = "wtnhxymk"
    let result = day05 input
    log $ "password: " <> show result.password
    log $ "password 2: " <> show result.password2
    equal "2414bc77" result.password
    equal "437e60fc" result.password2

examples :: forall e. Test e
examples = test "day 5 examples" do
    let result = day05 "abc"
    equal "18f47a30" result.password
    equal "05ace8e3" result.password2

main :: forall e. TestMain e
main = runTests [examples, testDay05]
