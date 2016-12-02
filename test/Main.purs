module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Node.FS (FS)
import Test.Assert (ASSERT)

import Test.Day01 (testDay01)

main :: forall e. Eff
            ( console :: CONSOLE
            , err :: EXCEPTION
            , fs :: FS
            , assert :: ASSERT
            | e) Unit
main = do
  log "Running all days..."
  testDay01
