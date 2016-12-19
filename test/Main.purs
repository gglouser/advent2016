module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Node.FS (FS)
import Test.Assert (ASSERT)

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

main :: forall e. Eff
            ( console :: CONSOLE
            , err :: EXCEPTION
            , fs :: FS
            , assert :: ASSERT
            | e) Unit
main = do
  log "Running all days..."
  testDay01
  testDay02
  testDay03
  testDay04
  -- testDay05  -- md5 too slow
  testDay06
  testDay07
  testDay08
  testDay09
  testDay10
  testDay11
  -- testDay12  -- a little slow
  testDay13
  -- testDay14  -- md5 too slow
  unit <$ testDay15
  unit <$ testDay16
  unit <$ testDay17
  unit <$ testDay18
