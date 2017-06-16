module Data.Uuid (Uuid(Uuid), USeed, uuidV4, nil)
       where


import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Exception.Unsafe (unsafeThrow)

import Data.Array
import Data.Maybe (fromMaybe)
import Data.Int.Bits

import Data.String (fromCharArray)



import Prelude

import Unsafe.Coerce (unsafeCoerce)

data Uuid = Uuid (Array Int)

data USeed = USeed (Array Int)

instance showSeed :: Show USeed where
  show (USeed is) = show is

instance showUuid :: Show Uuid where
  show = uuidToString



nil :: Uuid
nil = Uuid (replicate 32 0)

uuidV4 :: Array Int -> Uuid
uuidV4 i32s = Uuid (int128ToHexes i32s)

uuidToString :: Uuid -> String
uuidToString (Uuid xs) =
  ((take 8 >>> map toHex >>> fromCharArray) xs)
  <> "-"
  <> ((drop 8 >>> take 4 >>> map toHex >>> fromCharArray) xs)
  <> "-"
  <> ((drop 12 >>> take 4 >>> map toHex >>> fromCharArray) xs)
  <> "-"
  <> ((drop 16 >>> take 4 >>> map toHex >>> fromCharArray) xs)
  <> "-"
  <> ((drop 20 >>> take 12 >>> map toHex >>> fromCharArray) xs)
  

toHex :: Int -> Char
toHex x = case x of
  0 -> '0'
  1 -> '1'
  2 -> '2'
  3 -> '3'
  4 -> '4'
  5 -> '5'
  6 -> '6'
  7 -> '7'
  8 -> '8'
  9 -> '9'
  10 -> 'a'
  11 -> 'b'
  12 -> 'c'
  13 -> 'd'
  14 -> 'e'
  15 -> 'f'
  _ -> unsafeThrow ("not a hexadecimal: " <> show x)
  

int128ToHexes :: Array Int -> Array Int
int128ToHexes xs =
  let a1 = splitInt32 (fromMaybe 0 $ xs !! 0)
      a2 = setVersion $ splitInt32 (fromMaybe 0 $ xs !! 1)
      a3 = setHi $ splitInt32 (fromMaybe 0 $ xs !! 2) 
      a4 = splitInt32 (fromMaybe 0 $ xs !! 3)
  in a1 <> a2 <> a3 <> a4

setHi :: Array Int -> Array Int
setHi xs =
  let top = fromMaybe 0 $ xs !! 0
      top' = (top .&. 0x7) .|. 0x4
      -- top' = (top .|. 0x40000000)
      -- top' = (top .&. 0x7fffffff) .|. 0x40000000
  in fromMaybe xs (updateAt 0 top' xs)

setVersion :: Array Int -> Array Int
setVersion xs =
  let v = fromMaybe 0 $ xs !! 4
      v' = (v .&. 0x4) .|. 0x4
  in fromMaybe xs (updateAt 4 v' xs)

  

splitInt32 :: Int -> Array Int
splitInt32 i =
  let x1 = (i `zshr` 28) .&. 0x0000000f
      x2 = (i `zshr` 24) .&. 0x0000000f
      x3 = (i `zshr` 20) .&. 0x0000000f
      x4 = (i `zshr` 16) .&. 0x0000000f
      x5 = (i `zshr` 12) .&. 0x0000000f
      x6 = (i `zshr` 8) .&. 0x0000000f
      x7 = (i `zshr` 4) .&. 0x0000000f
      x8 = i .&. 0x0000000f
  in fromFoldable [x1, x2, x3, x4, x5, x6, x7, x8]


