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

data Guts a = Guts a (Double -> a -> a)

data ScriptF v next where
  -- these will yield
  ScAwait      :: Event a -> Double -> (Maybe [a] -> next) -> ScriptF v next
  ScAsyncIO    :: IO a -> (a -> next) -> ScriptF v next
  ScTerminate  :: ScriptF v next
  -- these happen immediately
  ScLook       :: View a -> (a -> next) -> ScriptF v next
  ScGuts       :: (Guts x -> (a, Guts x)) -> (a -> next) -> ScriptF x next
  ScTrigger    :: Port a -> a -> next -> ScriptF v next
  ScCheckpoint :: next -> ScriptF v next
  ScExec       :: IO () -> next -> ScriptF v next
  ScNewPort    :: ((Port a, Event a) -> next) -> ScriptF v next
  ScFork       :: Guts v
               -> Script v b
               -> ((Event (Maybe ()), View (Maybe v)) -> next)
               -> ScriptF u next

deriving instance Functor (ScriptF v)

type Script v a = Free (ScriptF v) a

instance MonadState v (FreeT (ScriptF v) Identity) where
  get = liftF (ScGuts f id) where
    f guts@(Guts x g) = (x, guts)
  put x' = liftF (ScGuts (\(Guts x g) -> ((), Guts x' g)) id) where

look :: View a -> Script v a
look v = liftF (ScLook v id)

getGuts :: Script v (Guts v)
getGuts = liftF (ScGuts (\s -> (s,s)) id)

putGuts :: Guts v -> Script v ()
putGuts guts = liftF (ScGuts (\_ -> ((),guts)) id)

modifyGuts :: (Guts a -> Guts a) -> Script a ()
modifyGuts f = (fmap f getGuts) >>= putGuts 

fork :: Guts v -> Script v b -> Script u (Event (Maybe ()), View (Maybe v))
fork guts scr = liftF (ScFork guts scr id)

await :: Event a -> Script v [a]
await e = liftF (ScAwait e (1/0) (id . fromJust))

timedAwait :: Event a -> Double -> Script v (Maybe [a])
timedAwait e dt = liftF (ScAwait e dt id)

sleep :: Double -> Script v ()
sleep dt = liftF (ScAwait never dt (const ()))

request :: IO a -> Script v a
request io = liftF (ScAsyncIO io id)

exec :: IO () -> Script v ()
exec io = liftF (ScExec io ())

trigger :: Port a -> a -> Script v ()
trigger k x = liftF (ScTrigger k x ())

newPort :: Script v (Port a, Event a)
newPort = liftF (ScNewPort id)

checkpoint :: Script v ()
checkpoint = liftF (ScCheckpoint ())

terminate :: Script v a
terminate = liftF ScTerminate

hang :: Script v a
hang = do
  await never
  terminate

