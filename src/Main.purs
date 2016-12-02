module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log "Hello, Advent of Code 2016!"
  log "Please run via the tests. For example: pulp test -m Test.Day01"
