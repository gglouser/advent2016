module Test.Day07 where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Node.FS (FS)
import Node.FS.Sync (readTextFile)
import Node.Encoding (Encoding(UTF8))
import Test.Assert (ASSERT, assert)
import Advent2016.Day07 (day07)

type Test a = forall e. Eff
                ( console :: CONSOLE
                , fs :: FS
                , err :: EXCEPTION
                , assert :: ASSERT
                | e) a

testDay07 :: Test Unit
testDay07 = do
    log "Running Day07"
    result <- day07 <$> readTextFile UTF8 "inputs/input07.txt"
    log $ "supporting TLS: " <> show result.countTLS
    log $ "supporting SSL: " <> show result.countSSL
    assert $ result.countTLS == 105
    assert $ result.countSSL == 258

testEx :: String -> Int -> Test Unit
testEx addr n = do
    let result = day07 addr
    assert $ result.countTLS == n

testEx2 :: String -> Int -> Test Unit
testEx2 addr n = do
    let result = day07 addr
    assert $ result.countSSL == n

examples :: Test Unit
examples = do
    log "Running day 07 examples"
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

main :: Test Unit
main = do
    examples
    testDay07
