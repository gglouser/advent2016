module Test.Main where

import Prelude
import Control.Monad.Eff.Console (log)
import Node.FS (FS)
import Test.Unit (TestMain, runTests)

import Test.Day01 (testDay01)
import Test.Day02 (testDay02)
import Test.Day03 (testDay03)
import Test.Day04 (testDay04)
-- import Test.Day05 (testDay05)
import Test.Day06 (testDay06)
import Test.Day07 (testDay07)
import Test.Day08 (testDay08)
import Test.Day09 (testDay09)
import Test.Day10 (testDay10)
import Test.Day11 (testDay11)
-- import Test.Day12 (testDay12)
import Test.Day13 (testDay13)
-- import Test.Day14 (testDay14)
import Test.Day15 (testDay15)
import Test.Day16 (testDay16)
import Test.Day17 (testDay17)
import Test.Day18 (testDay18)
import Test.Day19 (testDay19)
import Test.Day20 (testDay20)
import Test.Day21 (testDay21)
import Test.Day22 (testDay22)
import Test.Day23 (testDay23)
import Test.Day24 (testDay24)
import Test.Day25 (testDay25)

main :: forall e. TestMain (fs :: FS | e)
main = do
  log "Running all days..."
  runTests
    [ testDay01
    , testDay02
    , testDay03
    , testDay04
    -- , testDay05  -- md5 too slow
    , testDay06
    , testDay07
    , testDay08
    , testDay09
    , testDay10
    , testDay11  -- a little slow
    -- , testDay12  -- too slow
    , testDay13
    -- , testDay14  -- md5 too slow
    , testDay15
    , testDay16  -- a little slow
    , testDay17
    , testDay18  -- a little slow
    , testDay19
    , testDay20
    , testDay21
    , testDay22
    , testDay23
    , testDay24
    , testDay25
    ]
