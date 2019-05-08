module Effect.Uuid where

import Prelude (bind, ($), pure)

import Data.Uuid (uuidV4, Uuid)

import Effect (Effect)

foreign import _randomState :: Effect UuidInt

data UuidInt = Seed Int Int Int Int

genUuidV4 :: Effect Uuid
genUuidV4 = do
  Seed i1 i2 i3 i4 <- _randomState
  pure $ uuidV4 i1 i2 i3 i4
