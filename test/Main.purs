module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Effect.Aff.AVar

import Data.Uuid
import Data.Maybe
import Data.NonEmpty (NonEmpty(..))

import Data.Array as Array
import Data.Int.Bits

import Test.Unit (test)
import Test.Unit.Main (runTest)
-- import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.QuickCheck (quickCheck, quickCheck')

import Test.QuickCheck (Result(..), (===), (/==), arbitrary)
import Test.QuickCheck.Gen (Gen(..))
import Test.QuickCheck.Gen as Gen

v4HiProperty :: Int -> Int -> Int -> Int -> Result
v4HiProperty i1 i2 i3 i4 =
  let Uuid _ _ x _  = uuidV4 i1 i2 i3 i4
  in if ((x `zshr` 28) .&. 0x0000000f) < 4
     then Failed "Hi Property less than 4"
     else Success

v4VersionProperty :: Int -> Int -> Int -> Int -> Result
v4VersionProperty i1 i2 i3 i4 =
  let Uuid _ x _ _   = uuidV4 i1 i2 i3 i4
  in ((x `zshr` 12) .&. 0x0000000f) === 4


data UuidArg = IsUuid String
             | NotUuid String

isValidProperty :: Gen Result
isValidProperty = do
  args <- uuidArgs
  pure $ case args of
    IsUuid s -> isValid s === true
    NotUuid s -> isValid s /== true
  where
    uuidArgs = do
      let uid = uuidV4 <$> arbitrary <*> arbitrary
                <*> arbitrary <*> arbitrary
          uid' = (IsUuid <<< show) <$> uid
          s = NotUuid <$> arbitrary
      Gen.oneOf $ NonEmpty (uid') [uid', s]


parseProperty :: Int -> Int -> Int -> Int -> Result
parseProperty i1 i2 i3 i4 =
  let uid = uuidV4 i1 i2 i3 i4
  in case fromString (show uid) of
    Nothing -> Failed "not a valid uuid"
    Just x -> x === uid


main :: Effect  Unit
main = runTest do
  -- test "valid property" $ quickCheck validProperty
  test "isValid" $ quickCheck isValidProperty
  test "parse property" $ quickCheck parseProperty
  test "v4 Version property" $ quickCheck v4VersionProperty
  test "v4 Hi property" $ quickCheck v4HiProperty  
