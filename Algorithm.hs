{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Algorithm where

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad.RWS
import Data.HashSet (HashSet); import qualified Data.HashSet as HS
import Data.HashMap.Strict (HashMap); import qualified Data.HashMap.Strict as HM
import Control.Monad.Trans.Free

import View
import Process
import Signal
import Event
import Script
import Relation (Relation); import qualified Relation as D

data W sig a = W
  { wcounter :: Int
  , wroot    :: Pid a
  , wproctab :: ProcTab sig
  , wdisp    :: Dispatcher }

data DriverAction sig a =
  TimePass Double |
  Stimulus (Occurrences sig) |
  RenderDump |
  Answer (W sig a -> W sig a)

-- the dispatcher is a 2 way map between Pids and absolute wake up times.
type Dispatcher = Relation Int Double


newtype IOUnit = IOUnit { runIOUnit :: IO () }
instance Monoid IOUnit where
  mempty = IOUnit (return ())
  mappend (IOUnit io1) (IOUnit io2) = IOUnit (io1 >> io2)

-- the resolution algorithm monad
type Rez sig v a = RWS
  (Double, MVar (DriverAction sig v), ProcTab sig)
  IOUnit
  (Int, Int, ProcTab sig, HashSet Int, OccsBag sig, Dispatcher)
  a

data ProcStatus sig b =
  Marked |
  Blocked |
  Runnable |
  forall a s c. WaitingE (Event sig a) s (Guts b) (Maybe [a] -> Script sig s b c) |
  forall s c . WaitingT s (Guts b) (Script sig s b c)

-- given the current time, async mvar, current occurs, resolve the worlds
-- waiting processes and return an IO action of the effects
resolve
  :: forall sig v .
     Keys sig
  => Double
  -> MVar (DriverAction sig v)
  -> Occurrences sig
  -> W sig v
  -> (W sig v, IO ())
resolve now mv os0 w = (w', out) where
  w' = w { wcounter = c', wproctab = ps', wdisp = disp' }
  ((c', _, ps', _, _, disp'), IOUnit out) =
    execRWS (go os0) (now, mv, ps0) (wcounter w, 0, ps0, HS.empty, HM.empty, wdisp w)
  ps0 = wproctab w
  go :: Occurrences sig -> Rez sig v ()
  go os = do 
    clearOccsCount
    forEachProcess $ \pid p -> do
      pv <- analyzeProc pid p
      let (Pid i) = pid
      case pv of
        Marked -> return () -- do nothing
        Blocked -> return () -- do nothing
        Runnable -> do
          markProc i
          mp' <- runRunnable now pid p
          case mp' of
            Just p' -> updateProc pid p'
            Nothing -> deleteProc pid
        WaitingT st guts next -> do
          mt <- dispLookup1 i
          case mt of
            Just t -> if t == now
              then do
                markProc i
                dispDelete i
                mp' <- runRunnable now pid (Proc next guts st)
                case mp' of
                  Just p' -> updateProc pid p'
                  Nothing -> deleteProc pid
              else return ()
            Nothing -> error "sleeping process not found in records 1"
        WaitingE e st guts h -> do
          mt <- dispLookup1 i
          case mt of
            Just t -> if t == now
              then do
                markProc i
                dispDelete i
                mp' <- runRunnable now pid (Proc (h Nothing) guts st)
                case mp' of
                  Just p' -> updateProc pid p'
                  Nothing -> deleteProc pid
              else do
                tab <- getOriginalProcs
                case runEvent e (viewFixedPoint tab) os of
                  [] -> return ()
                  xs -> do
                    markProc i
                    mp' <- runRunnable now pid (Proc (h (Just xs)) guts st)
                    case mp' of
                      Just p' -> updateProc pid p'
                      Nothing -> deleteProc pid
            Nothing -> error "sleeping process not found in records 2"
    os' <- getOccs
    clearOccs
    c <- getOccsCount
    when (c > 0) (go os')

-- see if a proc is runnable, if its waiting for something, or if it
-- has already woken up and gone back to sleep so we can ignore it.
analyzeProc :: forall a sig v . Pid a -> Process sig a -> Rez sig v (ProcStatus sig a)
analyzeProc (Pid i) p@(Proc scr guts st) = answer where
  answer :: Rez sig v (ProcStatus sig a)
  answer = do
    marked <- isMarked i
    if marked
      then return Marked
      else return (f st scr)
  f :: forall s b . s -> Script sig s a b -> ProcStatus sig a
  f st scr = case runFree scr of
    Pure _                     -> Runnable
    Free (ScLook _ _)          -> Runnable
    Free (ScTrigger _ _ _)     -> Runnable
    Free (ScModify _ _)        -> Runnable
    Free (ScSetView _ _)       -> Runnable
    Free (ScFork1 _ _ _ _ _)   -> Runnable
    Free (ScFork2 _ _ _ _)     -> Runnable
    Free (ScGet _)             -> Runnable
    Free (ScPut _ _)           -> Runnable
    Free (ScExec _ _)          -> Runnable
    Free (ScWaitFor2 e _ next) -> WaitingE e st guts next
    Free (ScSleep _ next)      -> WaitingT st guts next
    Free (ScAsyncIO _ _)       -> Blocked
    Free ScTerminate           -> Runnable

-- run a runnable proc. has side effects. returns the latest version of the 
-- proc which is guaranteed to now be waiting for something, or Nothing if
-- the process ended.
runRunnable :: forall sig a v . Keys sig => Double -> Pid a -> Process sig a -> Rez sig v (Maybe (Process sig a))
runRunnable now pid@(Pid i) (Proc x y z) = fmap (fmap finalize) $ (go z y x) where
  finalize :: forall s b . (s, Guts a, Script sig s a b) -> Process sig a
  finalize (x,y,z) = Proc z y x
  go :: forall s b . s -> Guts a -> Script sig s a b -> Rez sig v (Maybe (s, Guts a, Script sig s a b))
  go st guts scr = case runFree scr of
    Pure _ -> return Nothing
    Free (ScLook v next) -> do
      tab <- getOriginalProcs
      let ps = viewFixedPoint tab
      let x = runView v ps
      go st guts (next x)
    Free (ScTrigger k x next) -> do
      incOccsCount 1
      emitOcc (SigN (toNumber k)) x
      go st guts next
    Free (ScModify f next) -> go st (modifyGuts f guts) next
    Free (ScSetView v next) -> go st (ViewGuts v) next
    Free (ScFork1 st' g x scr next) -> do
      v <- newProc (Proc scr (GenGuts g x) st')
      go st guts (next v)
    Free (ScFork2 st' v scr next) -> do
      v <- newProc (Proc scr (ViewGuts v) st')
      go st guts (next v)
    Free (ScGet next) -> go st guts (next st)
    Free (ScPut st' next) -> go st' guts next 
    Free (ScExec io next) -> do
      output io
      go st guts next
    Free (ScSleep dt next) -> do
      dispInsert i (now + dt)
      return (Just (st, guts, scr))
    Free (ScWaitFor2 e dt next) -> do
      dispInsert i (now + dt)
      return (Just (st, guts, scr))
    Free (ScAsyncIO io next) -> do
      outputAsyncRequest pid io (\ans -> Proc (next ans) guts st)
      return (Just (st, guts, scr)) -- should now be blocked
    Free ScTerminate -> return Nothing

-- aux commands

getOccsCount :: Rez sig v Int
getOccsCount = gets (\(c,x,y,z,w,d) -> x)

incOccsCount :: Int -> Rez sig v ()
incOccsCount n = modify (\(c,x,y,z,w,d) -> (c,x+n,y,z,w,d))

clearOccsCount :: Rez sig v ()
clearOccsCount = modify (\(c,x,y,z,w,d) -> (c,0,y,z,w,d))

getCurrentProcs :: Rez sig v (ProcTab sig)
getCurrentProcs = gets (\(c,x,y,z,w,d) -> y)

setCurrentProcs :: ProcTab sig -> Rez sig v ()
setCurrentProcs tab = modify (\(c,x,_,z,w,d) -> (c,x,tab,z,w,d))

deleteProc :: Pid a -> Rez sig v ()
deleteProc (Pid i) = modify (\(c,x,y,z,w,d) -> (c,x,HM.delete i y,z,w,d))

updateProc :: Pid a -> Process sig a -> Rez sig v ()
updateProc pid p = modify (\(c,x,y,z,w,d) -> (c,x,overwriteProc pid p y,z,w,d))

newProc :: Process sig a -> Rez sig v (View (Maybe a))
newProc p = do
  c <- takeCounter
  tab <- getCurrentProcs
  let pid = Pid c
  let tab' = HM.insert c (HideProc pid p) tab
  setCurrentProcs tab'
  return (viewPid pid)
  
takeCounter :: Rez sig v Int
takeCounter = state (\(c,x,y,z,w,d) -> (c, (c+1,x,y,z,w,d)))

emitOcc :: SigN sig a -> a -> Rez sig v ()
emitOcc s v = modify (\(c,x,y,z,bag,d) -> (c,x,y,z,appendOccs s v bag,d))

clearOccs :: Rez sig v ()
clearOccs = modify (\(c,x,y,z,bag,d) -> (c,x,y,z,HM.empty,d))

getOccs :: Rez sig v (Occurrences sig)
getOccs = gets (compileBag . (\(c,x,y,z,w,d) -> w))

markProc :: Int -> Rez sig v ()
markProc i = modify (\(c,x,y,z,w,d) -> (c,x,y,HS.insert i z,w,d))

isMarked :: Int -> Rez sig v Bool
isMarked i = gets (HS.member i . (\(c,x,y,z,w,d) -> z))

output :: IO () -> Rez sig v ()
output io = tell (IOUnit io)

outputAsyncRequest :: Pid b -> IO a -> (a -> Process sig b) -> Rez sig v ()
outputAsyncRequest pid io handler = do
  mv <- asks (\(x,y,z) -> y)
  output $ do
    forkIO (requestThread pid io handler (putMVar mv . Answer))
    return ()

dispInsert :: Int -> Double -> Rez sig v ()
dispInsert i t = modify (\(c,x,y,z,w,d) -> (c,x,y,z,w,D.insert i t d))

dispDelete :: Int -> Rez sig v ()
dispDelete i = modify (\(c,x,y,z,w,d) -> (c,x,y,z,w,D.deleteL i d))

dispLookup1 :: Int -> Rez sig v (Maybe Double)
dispLookup1 i = gets (\(c,x,y,z,w,d) -> D.lookupL1 i d)

getCurrentTime :: Rez sig v Double
getCurrentTime = asks (\(x,y,z) -> x)

getOriginalProcs :: Rez sig v (ProcTab sig)
getOriginalProcs = asks (\(x,y,z) -> z)

forEachProcess ::
  (forall a . Pid a -> Process sig a -> Rez sig v b) -> Rez sig v [b]
forEachProcess f = do
  ps <- getCurrentProcs
  forM (HM.toList ps) (\(i, HideProc pid p) -> f pid p)

---
--
--

answer :: Pid a -> Process sig a -> W sig b -> W sig b
answer pid pr w = w' where
  w' = w { wproctab = pt' }
  pt' = overwriteProc pid pr (wproctab w)

requestThread ::
  Pid b -> IO a -> (a -> Process sig b) -> ((W sig v -> W sig v) -> IO ()) -> IO ()
requestThread pid io handler doReply = do
  x <- io
  let p' = handler x
  doReply (answer pid p')

