module Event where

import Data.Maybe
import Data.Monoid

import Signal
import View

-- an event is a selection and filtering of occurrences
newtype Event a =
  Event { runEvent :: Processes -> Occurrences -> [a] }

instance Functor Event where
  fmap f (Event g) = Event (\ps os -> map f (g ps os))

instance Monoid (Event a) where
  mempty = Event (\_ _ -> [])
  Event f `mappend` Event g = Event (f `mappend` g)

never :: Event a
never = mempty

fmapMaybeE :: (a -> Maybe b) -> Event a -> Event b
fmapMaybeE f (Event g) = Event (\ps os -> catMaybes (map f (g ps os)))

filterE :: (a -> Bool) -> Event a -> Event a
filterE f = fmapMaybeE (\x -> if f x then Just x else Nothing)

trimE :: Event (Maybe a) -> Event a
trimE = fmapMaybeE id

onPort :: Port a -> Event a
onPort p = Event f where
  f _ os = occsLookup os p

-- WARNING suspicious
onCheckpoint :: Pid a -> Event (Maybe ())
onCheckpoint (Pid i) = Event f where
  f _ os = occsLookup os (IntPort i)

snap1 :: View a -> Event (a -> b) -> Event b
snap1 v e = Event f where
  f ps os = let x = runView v ps in fmap ($ x) (runEvent e ps os)

snap2 :: View (a -> b) -> Event a -> Event b
snap2 v e = Event f where
  f ps os = let g = runView v ps in fmap (g $) (runEvent e ps os)

eitherE :: Event a -> Event b -> Event (Either a b)
eitherE e1 e2 = fmap Left e1 <> fmap Right e2
