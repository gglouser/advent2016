module Test.Day11 where

import Prelude
import Control.Monad.Eff.Console (log)
import Data.Maybe (fromJust, isJust)
import Node.FS (FS)
import Node.FS.Sync (readTextFile)
import Node.Encoding (Encoding(UTF8))
import Partial.Unsafe (unsafePartial)
import Test.Unit (TestMain, Test, runTests, test, equal, assert)
import Advent2016.Day11 (day11, parse, part2Pairs)

testDay11 :: forall e. Test (fs :: FS | e)
testDay11 = test "day 11" do
    inputMB <- parse <$> readTextFile UTF8 "inputs/input11.txt"
    assert "parse is Just" $ isJust inputMB
    let input = unsafePartial $ fromJust inputMB
        resultMB = day11 input
    assert "result is Just" $ isJust resultMB
    let result = unsafePartial $ fromJust resultMB
    log $ "part1: " <> show result.moves
    equal 33 result.moves

    let result2MB = day11 $ input <> part2Pairs
    assert "result 2 is Just" $ isJust result2MB
    let result2 = unsafePartial $ fromJust result2MB
    log $ "part2: " <> show result2.moves
    equal 57 result2.moves

examples :: forall e. Test e
examples = test "day 11 examples" do
    let inputMB = parse "\
\The first floor contains a hydrogen-compatible microchip and a lithium-compatible microchip.\n\
\The second floor contains a hydrogen generator.\n\
\The third floor contains a lithium generator.\n\
\The fourth floor contains nothing relevant."
    assert "parse is Just" $ isJust inputMB
    let resultMB = unsafePartial $ day11 $ fromJust inputMB
    assert "result is Just" $ isJust resultMB
    let result = unsafePartial $ fromJust resultMB
    equal 11 result.moves

main :: forall e. TestMain (fs :: FS | e)
main = runTests [examples, testDay11]
