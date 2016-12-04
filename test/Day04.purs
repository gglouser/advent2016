module Test.Day04 where

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
import Advent2016.Day04 (day04, decryptName)

type Test a = forall e. Eff
                ( console :: CONSOLE
                , fs :: FS
                , err :: EXCEPTION
                , assert :: ASSERT
                | e) a

testDay04 :: Test Unit
testDay04 = unsafePartial do
    log "Running Day04"
    resultMB <- day04 <$> readTextFile UTF8 "inputs/input04.txt"
    assert $ isJust resultMB
    let result = fromJust resultMB
    log $ "real room sector ID sum: " <> show result.sectorIDSum
    log $ "North Pole object storage: " <> show result.npoStorage
    assert $ result.sectorIDSum == 409147
    assert $ result.npoStorage == [991]

examples :: Test Unit
examples = unsafePartial do
    log "Running day 4 examples"
    let resultMB = day04 "aaaaa-bbb-z-y-x-123[abxyz]\n\
                         \a-b-c-d-e-f-g-h-987[abcde]\n\
                         \not-a-real-room-404[oarel]\n\
                         \totally-real-room-200[decoy]\n"
    assert $ isJust resultMB
    let result = fromJust resultMB
    assert $ result.sectorIDSum == 1514
    assert $ decryptName {name:"qzmt-zixmtkozy-ivhz", sector:343, checksum:""}
                == "very encrypted name"

main :: Test Unit
main = do
    examples
    testDay04
