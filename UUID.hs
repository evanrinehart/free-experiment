{-# LANGUAGE DeriveGeneric #-}
module UUID where

-- not official UUIDs of any kind, good enough

import Data.Hashable
import GHC.Generics
import System.Random
import Control.Monad
import Data.Char
import Numeric

data UUID = UUID String deriving (Eq, Ord, Generic)

instance Hashable UUID

instance Show UUID where
  show (UUID bs) = concatMap (f . ord) bs where
    f n =
      let (q,r) = divMod n 16
      in showHex q "" ++ showHex r ""

newUUID :: IO UUID
newUUID = do
  bs <- replicateM 16 (randomRIO ('\0','\255'))
  return (UUID bs)
