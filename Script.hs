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

data Guts a =
  ViewGuts (View a) |
  GenGuts (Double -> a -> a) a

data ScriptF sig u v next where
  -- these will yield
  ScAwait      :: Event sig a -> Double -> (Maybe [a] -> next) -> ScriptF sig s v next
  ScAsyncIO    :: IO a -> (a -> next) -> ScriptF sig s v next
  ScTerminate  :: ScriptF sig s v next
  -- these happen immediately
  ScLook       :: View a -> (a -> next) -> ScriptF sig s v next
  ScGet        :: (s -> next) -> ScriptF sig s v next
  ScPut        :: s -> next -> ScriptF sig s v next
  ScModify     :: (Guts x -> Guts x) -> next -> ScriptF sig s x next
  ScTrigger    :: sig a -> a -> next -> ScriptF sig s v next
  ScCheckpoint :: next -> ScriptF sig s v next
  ScExec       :: IO () -> next -> ScriptF sig s v next
  ScFork       :: s
               -> Guts v
               -> ScriptSG sig s v b
               -> ((Event sig (Maybe ()), View (Maybe v)) -> next)
               -> ScriptF sig r u next

deriving instance Functor (ScriptF sig s v)

type ScriptSG sig s v a = Free (ScriptF sig s v) a

instance MonadState s (FreeT (ScriptF sig s v) Identity) where
  get = liftF (ScGet id)
  put x = liftF (ScPut x ())

look :: View a -> ScriptSG sig s v a
look v = liftF (ScLook v id)

modifyGuts :: (Guts a -> Guts a) -> ScriptSG sig s a ()
modifyGuts f = liftF (ScModify f ())

modifyGen :: (x -> x) -> ScriptSG sig s x ()
modifyGen f = modifyGuts g where
  g (ViewGuts _) = error "fix api"
  g (GenGuts h x) = GenGuts h (f x)

setView :: View v -> ScriptSG sig s v ()
setView v = modifyGuts (const (ViewGuts v))

fork :: s -> Guts v -> ScriptSG sig s v b
     -> ScriptSG sig r u (Event sig (Maybe ()), View (Maybe v))
fork s guts scr = liftF (ScFork s guts scr id)

await :: Event sig a -> ScriptSG sig s v [a]
await e = liftF (ScAwait e (1/0) (id . fromJust))

timedAwait :: Event sig a -> Double -> ScriptSG sig s v (Maybe [a])
timedAwait e dt = liftF (ScAwait e dt id)

sleep :: Double -> ScriptSG sig s v ()
sleep dt = liftF (ScAwait never dt (const ()))

request :: IO a -> ScriptSG sig s v a
request io = liftF (ScAsyncIO io id)

exec :: IO () -> ScriptSG sig s v ()
exec io = liftF (ScExec io ())

trigger :: sig a -> a -> ScriptSG sig s v ()
trigger k x = liftF (ScTrigger k x ())

terminate :: ScriptSG sig s v a
terminate = liftF ScTerminate

hang :: ScriptSG sig s v a
hang = do
  await never
  terminate

