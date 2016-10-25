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

-- A signal name with magic number saved
data OccSrcIx (sig :: * -> *) a = SigIx !Int | PidIx !Int
  deriving (Eq, Ord)

data SkelKey (sig :: * -> *) = SkSigIx !Int | SkPidIx !Int
  deriving (Eq, Ord, Generic)

instance Hashable (SkelKey sig)

-- Occurrences is a dictionary of things currently occurring.
data Occurrences sig = Occs { occsLookup :: forall a . OccSrcIx sig a -> [a] }

instance Monoid (Occurrences sig) where
  mempty = Occs (const [])
  Occs f `mappend` Occs g = Occs (f `mappend` g)

-- The parameter to Occurrences should be a GADT of enums
-- one for each signal. Each key should have a different number.
class Keys f where
  toNumber :: f a -> Int

skel :: OccSrcIx sig a -> SkelKey sig
skel (SigIx i) = SkSigIx i
skel (PidIx i) = SkPidIx i

keyEquals :: OccSrcIx f a -> OccSrcIx f b -> Maybe (a :~: b)
keyEquals (SigIx n1) (SigIx n2) = if n1 == n2
  then Just (unsafeCoerce Refl) -- relies on Keys instance being correct
  else Nothing
keyEquals (PidIx n1) (PidIx n2) = if n1 == n2
  then Just (unsafeCoerce Refl) -- relies on Keys instance being correct
  else Nothing
keyEquals _ _ = Nothing

-- create an occurs dictionary for a single signal, single value
singletonOcc :: forall sig a . Keys sig => sig a -> a -> Occurrences sig
singletonOcc k x = Occs f where
  s1 :: OccSrcIx sig a
  s1 = SigIx (toNumber k)
  f :: forall a . OccSrcIx sig a -> [a]
  f s2 = case keyEquals s1 s2 of
    Just Refl -> [x]
    Nothing   -> []

data HideOcc (sig :: * -> *) = forall a . HideOcc a
type OccsBag sig = HashMap (SkelKey sig) [HideOcc sig]

appendOccs :: OccSrcIx sig a -> a -> OccsBag sig -> OccsBag sig
appendOccs k x bag = HM.insertWith (++) (skel k) [HideOcc x] bag

compileBag :: forall sig . OccsBag sig -> Occurrences sig
compileBag hm = Occs f where
  f :: OccSrcIx sig a -> [a]
  f k = case HM.lookup (skel k) hm of
    Nothing -> []
    Just xs -> map unsafeCrack xs
  unsafeCrack (HideOcc x) = unsafeCoerce x
