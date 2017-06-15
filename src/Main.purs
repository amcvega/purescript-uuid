module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

import Data.Array (length)

import Data.Uuid
-- import Eff.Random
-- import Control.Monad.Eff.Random 

main :: ∀ e. Eff (crypto :: CRYPTO, console :: CONSOLE | e) Unit
main = do
  seed <- randomSeed
  log $ "Seeds: " <> show seed
  let Uuid ans =  uuidV4 seed -- (uuidV4 ([-0x1, -0x1, -0x1, -0x1]))
  log $ "Hello sailor! "  <> show ans
  log $ "Length: " <> show (length ans)
  log $ "UUID: " <> show (Uuid ans)
