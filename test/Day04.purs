module Test.Day04 where

import Prelude
import Control.Monad.Eff.Console (log)
import Data.Array (zipWithA)
import Data.Maybe (Maybe(..))
import Node.FS (FS)
import Node.FS.Sync (readTextFile)
import Node.Encoding (Encoding(UTF8))
import Test.Unit (TestMain, Test, runTests, test, equal, failure)
import Advent2016.Day04 (day04, decryptName, parse)

testDay04 :: forall e. Test (fs :: FS | e)
testDay04 = test "day 04" do
    inputMB <- parse <$> readTextFile UTF8 "inputs/input04.txt"
    case inputMB of
        Nothing -> failure "failed to parse"
        Just input -> do
            let result = day04 input
            log $ "real room sector ID sum: " <> show result.sectorIDSum
            equal 409147 result.sectorIDSum
            log $ "North Pole object storage: " <> show result.npoStorage
            equal [991] result.npoStorage

examples :: forall e. Test e
examples = test "day 04 examples" do
    let ex_input = "aaaaa-bbb-z-y-x-123[abxyz]\n\
                   \a-b-c-d-e-f-g-h-987[abcde]\n\
                   \not-a-real-room-404[oarel]\n\
                   \totally-real-room-200[decoy]\n"
        example = [ { name: "aaaaa-bbb-z-y-x", sector: 123, checksum: "abxyz" }
                  , { name: "a-b-c-d-e-f-g-h", sector: 987, checksum: "abcde" }
                  , { name: "not-a-real-room", sector: 404, checksum: "oarel" }
                  , { name: "totally-real-room", sector: 200, checksum: "decoy" }
                  ]
        equalRoom r1 r2 = do
            equal r1.name r2.name
            equal r1.sector r2.sector
            equal r1.checksum r2.checksum
    case parse ex_input of
        Nothing -> failure "example parse failed"
        Just rooms -> zipWithA equalRoom example rooms
    let result = day04 example
    equal 1514 result.sectorIDSum
    equal "very encrypted name" $
        decryptName {name:"qzmt-zixmtkozy-ivhz", sector:343, checksum:""}

main :: forall e. TestMain (fs :: FS | e)
main = runTests [examples, testDay04]
