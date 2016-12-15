module Test.Day11 where

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
import Advent2016.Day11 (day11, parse, part2Pairs)

type Test a = forall e. Eff
                ( console :: CONSOLE
                , fs :: FS
                , err :: EXCEPTION
                , assert :: ASSERT
                | e) a

testDay11 :: Test Unit
testDay11 = unsafePartial do
    log "Running Day11"
    inputMB <- parse <$> readTextFile UTF8 "inputs/input11.txt"
    assert $ isJust inputMB
    let input = fromJust inputMB
        resultMB = day11 input
    assert $ isJust resultMB
    let result = fromJust resultMB
    log $ "part1: " <> show result.moves
    assert $ result.moves == 33

    let result2MB = day11 $ input <> part2Pairs
    assert $ isJust result2MB
    let result2 = fromJust result2MB
    log $ "part2: " <> show result2.moves
    assert $ result2.moves == 57

examples :: Test Unit
examples = unsafePartial do
    log "Running day 11 examples"
    let inputMB = parse "\
\The first floor contains a hydrogen-compatible microchip and a lithium-compatible microchip.\n\
\The second floor contains a hydrogen generator.\n\
\The third floor contains a lithium generator.\n\
\The fourth floor contains nothing relevant."
    assert $ isJust inputMB
    let resultMB = day11 $ fromJust inputMB
    assert $ isJust resultMB
    let result = fromJust resultMB
    assert $ result.moves == 11

main :: Test Unit
main = do
    examples
    testDay11
