module Test.Day04 where

import Prelude
import Control.Monad.Eff.Console (log)
import Data.Maybe (fromJust, isJust)
import Node.FS (FS)
import Node.FS.Sync (readTextFile)
import Node.Encoding (Encoding(UTF8))
import Partial.Unsafe (unsafePartial)
import Test.Unit (TestMain, Test, runTests, test, equal, assert)
import Advent2016.Day04 (day04, decryptName)

testDay04 :: forall e. Test (fs :: FS | e)
testDay04 = test "day 04" do
    resultMB <- day04 <$> readTextFile UTF8 "inputs/input04.txt"
    assert "result is Just" $ isJust resultMB
    let result = unsafePartial $ fromJust resultMB
    log $ "real room sector ID sum: " <> show result.sectorIDSum
    log $ "North Pole object storage: " <> show result.npoStorage
    equal 409147 result.sectorIDSum
    equal [991] result.npoStorage

examples :: forall e. Test e
examples = test "day 04 examples" do
    let resultMB = day04 "aaaaa-bbb-z-y-x-123[abxyz]\n\
                         \a-b-c-d-e-f-g-h-987[abcde]\n\
                         \not-a-real-room-404[oarel]\n\
                         \totally-real-room-200[decoy]\n"
    assert "result is Just" $ isJust resultMB
    let result = unsafePartial $ fromJust resultMB
    equal 1514 result.sectorIDSum
    equal "very encrypted name" $
        decryptName {name:"qzmt-zixmtkozy-ivhz", sector:343, checksum:""}

main :: forall e. TestMain (fs :: FS | e)
main = runTests [examples, testDay04]
