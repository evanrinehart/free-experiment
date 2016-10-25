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

import View
import Event

data ScriptF sig u v next where
  -- these happen immediately
  ScLook      :: View a -> (a -> next) -> ScriptF sig s v next
  ScModify    :: (x -> x) -> next -> ScriptF sig s x next
  ScSetView   :: View v -> next -> ScriptF sig s v next
  ScFork1     :: s' -> (Double -> x -> x) -> x -> Script sig s' x a -> (View (Maybe x) -> next) -> ScriptF sig s v next
  ScFork2     :: s' -> View v' -> Script sig s' v' a -> (View (Maybe v') -> next) -> ScriptF sig s v next 
  ScTrigger   :: sig a -> a -> next -> ScriptF sig s v next
  ScGet :: (s -> next) -> ScriptF sig s v next
  ScPut :: s -> next -> ScriptF sig s v next
  -- these will yield
  ScWaitFor1  :: Event sig a -> ([a] -> next) -> ScriptF sig s v next
  ScWaitFor2  :: Event sig a -> Double -> (Maybe [a] -> next) -> ScriptF sig s v next
  ScSleep     :: Double -> next -> ScriptF sig s v next
  ScAsyncIO   :: IO a -> (a -> next) -> ScriptF sig s v next
  ScExec      :: IO () -> next -> ScriptF sig s v next
  --
  ScTerminate :: ScriptF sig s v next

deriving instance Functor (ScriptF sig s v)

type Script sig s v a = Free (ScriptF sig s v) a

instance MonadState s (FreeT (ScriptF sig s v) Identity) where
  get = liftF (ScGet id)
  put x = liftF (ScPut x ())

look :: View a -> Script sig s v a
look v = liftF (ScLook v id)

modifyGen :: (x -> x) -> Script sig s x ()
modifyGen f = liftF (ScModify f ())

setView :: View v -> Script sig s v ()
setView v = liftF (ScSetView v ())

fork1 :: s' -> (Double -> x -> x) -> x -> Script sig s' x a -> Script sig s v (View (Maybe x))
fork1 s f x scr = liftF (ScFork1 s f x scr id)

fork2 :: s' -> View u -> Script sig s' u a -> Script sig s v (View (Maybe u))
fork2 s v scr = liftF (ScFork2 s v scr id)

await :: Event sig a -> Script sig s v [a]
await e = liftF (ScWaitFor1 e id)

timedAwait :: Event sig a -> Double -> Script sig s v (Maybe [a])
timedAwait e dt = liftF (ScWaitFor2 e dt id)

sleep :: Double -> Script sig s v ()
sleep dt = liftF (ScSleep dt ())

request :: IO a -> Script sig s v a
request io = liftF (ScAsyncIO io id)

exec :: IO () -> Script sig s v ()
exec io = liftF (ScExec io ())

trigger :: sig a -> a -> Script sig s v ()
trigger k x = liftF (ScTrigger k x ())
--exec :: IO a -> Script sig v ()

terminate :: Script sig s v a
terminate = liftF ScTerminate

-- if we have a Script Picture ()
-- then we can run it by creating an empty process table
-- and empty signal table

{-
runScript :: TestEquality ch => Script ch a -> DMap ch -> (DSet ch, Maybe a)
runScript prog dm = go prog dempty where
  go p set = case runFree p of
    Pure x -> (set, Just x)
    Free (ScReject _) -> (set, Nothing)
    Free (ScPunt  _) -> (set, Nothing)
    Free (ScRead k next) ->
      let set' = dappend k set in
      let v = lookup k dm in
      go (next v) set'
-}
