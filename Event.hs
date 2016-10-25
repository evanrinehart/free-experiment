module Event where

import Data.Maybe

import Signal
import View

-- an event is a selection and filtering of occurrences
newtype Event sig a =
  Event { runEvent :: Processes -> Occurrences sig -> [a] }

instance Functor (Event sig) where
  fmap f (Event g) = Event (\ps os -> map f (g ps os))

instance Monoid (Event sig a) where
  mempty = Event (\_ _ -> [])
  Event f `mappend` Event g = Event (f `mappend` g)

never :: Event sig a
never = mempty

fmapMaybeE :: (a -> Maybe b) -> Event sig a -> Event sig b
fmapMaybeE f (Event g) = Event (\ps os -> catMaybes (map f (g ps os)))

filterE :: (a -> Bool) -> Event sig a -> Event sig a
filterE f = fmapMaybeE (\x -> if f x then Just x else Nothing)

trimE :: Event sig (Maybe a) -> Event sig a
trimE = fmapMaybeE id

onSignal :: Keys sig => sig a -> Event sig a
onSignal k = Event f where
  s = SigIx (toNumber k)
  f ps os = occsLookup os s

-- event occurs with value Just () when the given process executes `checkpoint'
onCheckpoint :: Pid b -> Event sig (Maybe ())
onCheckpoint (Pid i) = Event f where
  f ps os = occsLookup os (PidIx i)

snap1 :: View a -> Event sig (a -> b) -> Event sig b
snap1 v e = Event f where
  f ps os = let x = runView v ps in fmap ($ x) (runEvent e ps os)

snap2 :: View (a -> b) -> Event sig a -> Event sig b
snap2 v e = Event f where
  f ps os = let g = runView v ps in fmap (g $) (runEvent e ps os)
