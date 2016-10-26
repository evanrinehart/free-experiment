{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Script where

import Control.Monad.Trans.Free
import Control.Monad.State
import Control.Monad.Identity
import Prelude hiding (lookup)
import Data.Maybe

import View
import Event
import Signal

data Guts a =
  ViewGuts (View a) |
  GenGuts (Double -> a -> a) a

data ScriptF s v next where
  -- these will yield
  ScAwait      :: Event a -> Double -> (Maybe [a] -> next) -> ScriptF s v next
  ScAsyncIO    :: IO a -> (a -> next) -> ScriptF s v next
  ScTerminate  :: ScriptF s v next
  -- these happen immediately
  ScLook       :: View a -> (a -> next) -> ScriptF s v next
  ScGet        :: (s -> next) -> ScriptF s v next
  ScPut        :: s -> next -> ScriptF s v next
  ScModify     :: (Guts x -> Guts x) -> next -> ScriptF s x next
  ScTrigger    :: Port a -> a -> next -> ScriptF s v next
  ScCheckpoint :: next -> ScriptF s v next
  ScExec       :: IO () -> next -> ScriptF s v next
  ScNewPort    :: ((Port a, Event a) -> next) -> ScriptF s v next
  ScFork       :: s
               -> Guts v
               -> ScriptS s v b
               -> ((Event (Maybe ()), View (Maybe v)) -> next)
               -> ScriptF r u next

deriving instance Functor (ScriptF s v)

type ScriptS s v a = Free (ScriptF s v) a
type Script v a = ScriptS () v a

instance MonadState s (FreeT (ScriptF s v) Identity) where
  get = liftF (ScGet id)
  put x = liftF (ScPut x ())

look :: View a -> ScriptS s v a
look v = liftF (ScLook v id)

modifyGuts :: (Guts a -> Guts a) -> ScriptS s a ()
modifyGuts f = liftF (ScModify f ())

modifyGen :: (x -> x) -> ScriptS s x ()
modifyGen f = modifyGuts g where
  g (ViewGuts _) = error "fix api"
  g (GenGuts h x) = GenGuts h (f x)

setView :: View v -> ScriptS s v ()
setView v = modifyGuts (const (ViewGuts v))

fork :: s -> Guts v -> ScriptS s v b
     -> ScriptS r u (Event (Maybe ()), View (Maybe v))
fork s guts scr = liftF (ScFork s guts scr id)

await :: Event a -> ScriptS s v [a]
await e = liftF (ScAwait e (1/0) (id . fromJust))

timedAwait :: Event a -> Double -> ScriptS s v (Maybe [a])
timedAwait e dt = liftF (ScAwait e dt id)

sleep :: Double -> ScriptS s v ()
sleep dt = liftF (ScAwait never dt (const ()))

request :: IO a -> ScriptS s v a
request io = liftF (ScAsyncIO io id)

exec :: IO () -> ScriptS s v ()
exec io = liftF (ScExec io ())

trigger :: Port a -> a -> ScriptS s v ()
trigger k x = liftF (ScTrigger k x ())

newPort :: ScriptS s v (Port a, Event a)
newPort = liftF (ScNewPort id)

checkpoint :: ScriptS s v ()
checkpoint = liftF (ScCheckpoint ())

terminate :: ScriptS s v a
terminate = liftF ScTerminate

hang :: ScriptS s v a
hang = do
  await never
  terminate

