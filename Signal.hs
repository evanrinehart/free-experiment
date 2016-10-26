{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
module Signal where

import Data.HashMap.Strict (HashMap); import qualified Data.HashMap.Strict as HM
import Data.Hashable
import GHC.Generics
import Data.Type.Equality
import Unsafe.Coerce

import UUID

data Port a = ExtPort UUID | IntPort !Int
data Key    = ExtKey  UUID | IntKey  !Int
  deriving (Eq,Generic)

instance Hashable Key

-- Occurrences is a dictionary of things currently occurring.
data Occurrences = Occs { occsLookup :: forall a . Port a -> [a] }

instance Monoid Occurrences where
  mempty = Occs (const [])
  Occs f `mappend` Occs g = Occs (f `mappend` g)

skel :: Port a -> Key
skel (ExtPort i) = ExtKey i
skel (IntPort i) = IntKey i

keyEquals :: Port a -> Port b -> Maybe (a :~: b)
keyEquals (ExtPort u1) (ExtPort u2) = if u1 == u2
  then Just (unsafeCoerce Refl)
  else Nothing
keyEquals (IntPort i1) (IntPort i2) = if i1 == i2
  then Just (unsafeCoerce Refl)
  else Nothing
keyEquals _ _ = Nothing

-- create an occurs dictionary for a single signal, single value
singletonOcc :: Port a -> a -> Occurrences
singletonOcc p1 x = Occs f where
  f :: Port b -> [b]
  f p2 = case keyEquals p1 p2 of
    Just Refl -> [x]
    Nothing   -> []

data HideOcc = forall a . HideOcc a
type OccsBag = HashMap Key [HideOcc]

appendOccs :: Port a -> a -> OccsBag -> OccsBag
appendOccs k x bag = HM.insertWith (++) (skel k) [HideOcc x] bag

compileBag :: OccsBag -> Occurrences
compileBag hm = Occs f where
  f :: Port a -> [a]
  f k = case HM.lookup (skel k) hm of
    Nothing -> []
    Just xs -> map unsafeCrack xs
  unsafeCrack (HideOcc x) = unsafeCoerce x
