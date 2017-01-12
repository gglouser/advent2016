module Test.Day07 where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Node.FS (FS)
import Node.FS.Sync (readTextFile)
import Node.Encoding (Encoding(UTF8))
import Test.Unit (TestMain, Test, runTests, test, equal)
import Advent2016.Day07 (day07)

testDay07 :: forall e. Test (fs :: FS | e)
testDay07 = test "day 07" do
    result <- day07 <$> readTextFile UTF8 "inputs/input07.txt"
    log $ "supporting TLS: " <> show result.countTLS
    log $ "supporting SSL: " <> show result.countSSL
    equal 105 result.countTLS
    equal 258 result.countSSL

testEx :: forall e. String -> Int -> Eff (console :: CONSOLE, err :: EXCEPTION | e) Unit
testEx addr n = do
    let result = day07 addr
    equal n result.countTLS

testEx2 :: forall e. String -> Int -> Eff (console :: CONSOLE, err :: EXCEPTION | e) Unit
testEx2 addr n = do
    let result = day07 addr
    equal n result.countSSL

examples :: forall e. Test e
examples = test "day 07 examples" do
    testEx "abba[mnop]qrst" 1
    testEx "abcd[bddb]xyyx" 0
    testEx "aaaa[qwer]tyui" 0
    testEx "ioxxoj[asdfgh]zxcvbn" 1

    testEx2 "aba[bab]xyz" 1
    testEx2 "xyx[xyx]xyx" 0
    testEx2 "aaa[kek]eke" 1
    testEx2 "zazbz[bzb]cdb" 1

    -- gkg in sup, kgk in hyp,
    -- gkg ALSO in hyp but that is not disallowed as long as gkg is in sup
    testEx2 "gkg[kgk]iii[gkg]jjj" 1

main :: forall e. TestMain (fs :: FS | e)
main = runTests [examples, testDay07]
