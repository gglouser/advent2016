module Test.Day08 where

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
import Advent2016.Day08 (day08)

type Test a = forall e. Eff
                ( console :: CONSOLE
                , fs :: FS
                , err :: EXCEPTION
                , assert :: ASSERT
                | e) a

testDay08 :: Test Unit
testDay08 = unsafePartial do
    log "Running Day08"
    resultMB <- day08 50 6 <$> readTextFile UTF8 "inputs/input08.txt"
    assert $ isJust resultMB
    let result = fromJust resultMB
    log $ "num on: " <> show result.numOn
    assert $ result.numOn == 123
    log $ "final screen: "
    log result.finalScreen

examples :: Test Unit
examples = unsafePartial do
    log "Running day 08 examples"
    let resultMB = day08 7 3
                    "rect 3x2\n\
                    \rotate column x=1 by 1\n\
                    \rotate row y=0 by 4\n\
                    \rotate column x=1 by 1\n"
    assert $ isJust resultMB
    let result = fromJust resultMB
    assert $ result.numOn == 6
    assert $ result.finalScreen == ".#..#.#\n#.#....\n.#.....\n"

main :: Test Unit
main = do
    examples
    testDay08
