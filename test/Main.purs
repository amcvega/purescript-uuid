module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Random (RANDOM)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Aff.AVar

import Data.Uuid
import Data.Maybe

import Test.Unit (test)
import Test.Unit.Main (runTest)
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.QuickCheck (quickCheck)

import Test.QuickCheck (Result(..), (===))


validProperty :: Int -> Int -> Int -> Int -> Result
validProperty i1 i2 i3 i4 =
  let uid = uuidV4 i1 i2 i3 i4
  in isValid (show uid) === true


parseProperty :: Int -> Int -> Int -> Int -> Result
parseProperty i1 i2 i3 i4 =
  let uid = uuidV4 i1 i2 i3 i4
  in case fromString (show uid) of
    Nothing -> Failed "not a valid uuid"
    Just x -> x === uid


main :: forall e. Eff (random :: RANDOM
                      , console :: CONSOLE
                      , avar :: AVAR
                      , testOutput :: TESTOUTPUT | e) Unit
main = runTest do
  test "valid property" $ quickCheck validProperty
  test "parse property" $ quickCheck parseProperty


-- main = do
--   log "You should add some tests."
