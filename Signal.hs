{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
module Signal where

import Data.HashMap.Strict (HashMap); import qualified Data.HashMap.Strict as HM
import Data.Type.Equality
import Unsafe.Coerce

-- A signal name with magic number saved
data SigN (sig :: * -> *) a = SigN !Int

-- Occurrences is a dictionary things currently occurring.
data Occurrences sig = Occs { occsLookup :: forall a . SigN sig a -> [a] }

instance Monoid (Occurrences sig) where
  mempty = Occs (const [])
  Occs f `mappend` Occs g = Occs (f `mappend` g)

-- The parameter to Occurrences should be a GADT of enums
-- one for each signal. Each key should have a different number.
class Keys f where
  toNumber :: f a -> Int

keyEquals :: SigN f a -> SigN f b -> Maybe (a :~: b)
keyEquals (SigN n1) (SigN n2) = if n1 == n2
  then Just (unsafeCoerce Refl) -- relies on Keys instance being correct
  else Nothing

-- create an occurs dictionary for a single signal, single value
singletonOcc :: forall sig a . Keys sig => sig a -> a -> Occurrences sig
singletonOcc k x = Occs f where
  s1 :: SigN sig a
  s1 = SigN (toNumber k)
  f :: forall a . SigN sig a -> [a]
  f s2 = case keyEquals s1 s2 of
    Just Refl -> [x]
    Nothing   -> []

data HideOcc (sig :: * -> *) = forall a . HideOcc a
type OccsBag sig = HashMap Int [HideOcc sig]

appendOccs :: SigN sig a -> a -> OccsBag sig -> OccsBag sig
appendOccs k@(SigN i) x bag = HM.insertWith (++) i [HideOcc x] bag

compileBag :: OccsBag sig -> Occurrences sig
compileBag hm = Occs f where
  f :: SigN sig a -> [a]
  f (SigN i) = case HM.lookup i hm of
    Nothing -> []
    Just xs -> map unsafeCrack xs
  unsafeCrack (HideOcc x) = unsafeCoerce x
