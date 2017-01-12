module Test.Unit
( TestMain
, Test
, runTests
, test
, failure
, assert
, equal
) where

-- Bare-bones unit testing support.

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION, throwException, catchException, error, message)
import Data.Array (filter, length)
import Data.Traversable (sequence)
import Node.Process (PROCESS, exit)

type TestMain eff = Eff (console :: CONSOLE, process :: PROCESS | eff) Unit
type Test eff = Eff (console :: CONSOLE | eff) Boolean

runTests :: forall eff. Array (Test (process :: PROCESS | eff))
    -> TestMain eff
runTests tests = do
    successes <- sequence tests
    let total = length successes
        passed = length (filter id successes)
    log ("FINISHED - " <> show passed <> "/" <> show total <> " passed")
    when (passed /= total) do
        exit 1

test :: forall eff. String -> Eff (err :: EXCEPTION, console :: CONSOLE | eff) Unit
    -> Test eff
test lbl t = catchException failed runIt
    where
        runIt = do log $ "running " <> lbl
                   t $> true
        failed e = log (lbl <> " failed: " <> message e) $> false

failure :: forall a eff. String -> Eff (err :: EXCEPTION | eff) a
failure msg = throwException (error msg)

assert :: forall eff. String -> Boolean -> Eff (err :: EXCEPTION | eff) Unit
assert msg p = unless p $ failure msg

equal :: forall eff a. (Eq a, Show a) => a -> a -> Eff (err :: EXCEPTION | eff) Unit
equal expected actual =
    unless (expected == actual) $
        failure $ "expected " <> show expected <> ", got " <> show actual
