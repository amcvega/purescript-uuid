module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Random (RANDOM)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Aff.AVar

import Data.Uuid
import Data.Maybe

import Data.Array as Array

import Test.Unit (test)
import Test.Unit.Main (runTest)
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.QuickCheck (quickCheck)

import Test.QuickCheck (Result(..), (===))

v4HiProperty :: Int -> Int -> Int -> Int -> Result
v4HiProperty i1 i2 i3 i4 =
  let Uuid xs  = uuidV4 i1 i2 i3 i4
  in case Array.index xs 16 of
        Nothing  -> Failed "invalid uuid"
        Just x -> if x < 4
                  then Failed "Hi Property less than 4"
                  else Success
  

v4VersionProperty :: Int -> Int -> Int -> Int -> Result
v4VersionProperty i1 i2 i3 i4 =
  let Uuid xs  = uuidV4 i1 i2 i3 i4
  in case Array.index xs 12 of
        Nothing  -> Failed "invalid uuid"
        Just x -> x === 4

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
  test "v4 Version property" $ quickCheck v4VersionProperty
  test "v4 Hi property" $ quickCheck v4HiProperty  


-- main = do
--   log "You should add some tests."
