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

data W a = W
  { wcounter :: Int
  , wroot    :: Pid a
  , wproctab :: ProcTab
  , wdisp    :: Dispatcher
  , wrunlist :: HashSet Int }

data DriverAction a =
  TimePass Double |
  Stimulus Occurrences |
  RenderDump |
  Answer (W a -> W a)

-- the dispatcher is a 2 way map between Pids and absolute wake up times.
type Dispatcher = Relation Int Double


newtype IOUnit = IOUnit { runIOUnit :: IO () }
instance Monoid IOUnit where
  mempty = IOUnit (return ())
  mappend (IOUnit io1) (IOUnit io2) = IOUnit (io1 >> io2)

-- the resolution algorithm monad
type Rez v a = RWS
  (Double, MVar (DriverAction v), ProcTab)
  IOUnit
  (Int, Bool, ProcTab, HashSet Int, OccsBag, Dispatcher, HashSet Int)
  a

data ProcStatus b =
  Marked |
  Blocked |
  Runnable |
  forall a s c. WaitingE (Event a) s (Guts b) (Maybe [a] -> ScriptS s b c) |
  forall s c . WaitingT s (Guts b) (ScriptS s b c)

-- given the current time, async mvar, current occurs, resolve the worlds
-- waiting processes and return an IO action of the effects
resolve
  :: forall v . Double
  -> MVar (DriverAction v)
  -> Occurrences
  -> W v
  -> (W v, IO ())
resolve now mv os0 w = (w', out) where
  w' = w
    { wcounter = c'
    , wproctab = ps'
    , wdisp = disp'
    , wrunlist = runs' }
  ((c', _, ps', _, _, disp', runs'), IOUnit out) =
    execRWS (go os0) (now, mv, ps0)
      (wcounter w, False, ps0, HS.empty, HM.empty, wdisp w, wrunlist w)
  ps0 = wproctab w
  go :: Occurrences -> Rez v ()
  go os = do 
    clearActivityFlag
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
          clearRunnable i
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
    activity <- getActivityFlag
    when activity (go os')

-- see if a proc is runnable, if its waiting for something, or if it
-- has already woken up and gone back to sleep so we can ignore it.
analyzeProc :: forall a v . Pid a -> Process a -> Rez v (ProcStatus a)
analyzeProc (Pid i) p@(Proc scr guts st) = answer where
  answer :: Rez v (ProcStatus a)
  answer = do
    marked <- isMarked i
    if marked
      then return Marked
      else do
        runnable <- checkRunnable i
        if runnable
          then return Runnable
          else return (f st scr)
  f :: forall s b . s -> ScriptS s a b -> ProcStatus a
  f st scr = case runFree scr of
    Free (ScAwait e _ next) -> WaitingE e st guts next
    Free (ScAsyncIO _ _)       -> Blocked
    _ -> error "bug 2"

-- PROBLEM: the way the algorithm works is slightly convoluted. as a result
-- forking doesnt totally work. if a process is spawned which starts with a
-- wait command, then the next round will think that the wait has already 
-- been executed. if its a request, the request will not be issued. if its
-- a timed wait, it will check the dispatcher to see find it missing and
-- crash. solution: _

-- run a runnable proc. has side effects. returns the latest version of the 
-- proc which is guaranteed to now be waiting for something, or Nothing if
-- the process ended.
runRunnable :: forall a v . Double -> Pid a -> Process a -> Rez v (Maybe (Process a))
runRunnable now pid@(Pid i) (Proc x y z) = fmap (fmap finalize) $ (go z y x) where
  finalize :: forall s b . (s, Guts a, ScriptS s a b) -> Process a
  finalize (x,y,z) = Proc z y x
  go :: forall s b . s -> Guts a -> ScriptS s a b -> Rez v (Maybe (s, Guts a, ScriptS s a b))
  go st guts scr = case runFree scr of
    Pure _ -> return Nothing
    Free (ScLook v next) -> do
      tab <- getOriginalProcs
      let ps = viewFixedPoint tab
      let x = runView v ps
      go st guts (next x)
    Free (ScTrigger port x next) -> do
      setActivityFlag
      emitOcc port x
      go st guts next
    Free (ScCheckpoint next) -> do
      setActivityFlag
      emitOcc (IntPort i) (Just ())
      go st guts next
    Free (ScNewPort next) -> do
      i <- takeCounter
      let port = IntPort i
      go st guts (next (port, onPort port))
    Free (ScModify f next) -> go st (f guts) next
    Free (ScFork st' guts' scr next) -> do
      (i,v) <- newProc (Proc scr guts' st')
      setRunnable i
      setActivityFlag
      go st guts (next (onCheckpoint (Pid i), v))
    Free (ScGet next) -> go st guts (next st)
    Free (ScPut st' next) -> go st' guts next 
    Free (ScExec io next) -> do
      output io
      go st guts next
    Free (ScAwait e dt next) -> do
      dispInsert i (now + dt)
      return (Just (st, guts, scr))
    Free (ScAsyncIO io next) -> do
      outputAsyncRequest pid io (\ans -> Proc (next ans) guts st)
      return (Just (st, guts, scr)) -- should now be blocked
    Free ScTerminate -> do
      setActivityFlag
      emitOcc (IntPort i) Nothing
      return Nothing

-- aux commands

getActivityFlag :: Rez v Bool
getActivityFlag = gets (\(c,x,y,z,w,d,r) -> x)

setActivityFlag :: Rez v ()
setActivityFlag = modify (\(c,_,y,z,w,d,r) -> (c,True,y,z,w,d,r))

clearActivityFlag :: Rez v ()
clearActivityFlag = modify (\(c,_,y,z,w,d,r) -> (c,False,y,z,w,d,r))

getCurrentProcs :: Rez v ProcTab
getCurrentProcs = gets (\(c,x,y,z,w,d,r) -> y)

setCurrentProcs :: ProcTab -> Rez v ()
setCurrentProcs tab = modify (\(c,x,_,z,w,d,r) -> (c,x,tab,z,w,d,r))

deleteProc :: Pid a -> Rez v ()
deleteProc (Pid i) = modify (\(c,x,y,z,w,d,r) -> (c,x,HM.delete i y,z,w,d,r))

updateProc :: Pid a -> Process a -> Rez v ()
updateProc pid p = modify (\(c,x,y,z,w,d,r) -> (c,x,overwriteProc pid p y,z,w,d,r))

newProc :: Process a -> Rez v (Int, View (Maybe a))
newProc p = do
  c <- takeCounter
  tab <- getCurrentProcs
  let pid = Pid c
  let tab' = HM.insert c (HideProc pid p) tab
  setCurrentProcs tab'
  return (c, viewPid pid)
  
takeCounter :: Rez v Int
takeCounter = state (\(c,x,y,z,w,d,r) -> (c, (c+1,x,y,z,w,d,r)))

emitOcc :: Port a -> a -> Rez v ()
emitOcc s v = modify (\(c,x,y,z,bag,d,r) -> (c,x,y,z,appendOccs s v bag,d,r))

clearOccs :: Rez v ()
clearOccs = modify (\(c,x,y,z,bag,d,r) -> (c,x,y,z,HM.empty,d,r))

getOccs :: Rez v Occurrences
getOccs = gets (compileBag . (\(c,x,y,z,w,d,r) -> w))

markProc :: Int -> Rez v ()
markProc i = modify (\(c,x,y,z,w,d,r) -> (c,x,y,HS.insert i z,w,d,r))

isMarked :: Int -> Rez v Bool
isMarked i = gets (HS.member i . (\(c,x,y,z,w,d,r) -> z))

output :: IO () -> Rez v ()
output io = tell (IOUnit io)

outputAsyncRequest :: Pid b -> IO a -> (a -> Process b) -> Rez v ()
outputAsyncRequest pid io handler = do
  mv <- asks (\(x,y,z) -> y)
  output $ do
    forkIO (requestThread pid io handler (putMVar mv . Answer))
    return ()

dispInsert :: Int -> Double -> Rez v ()
dispInsert i t = modify (\(c,x,y,z,w,d,r) -> (c,x,y,z,w,D.insert i t d,r))

dispDelete :: Int -> Rez v ()
dispDelete i = modify (\(c,x,y,z,w,d,r) -> (c,x,y,z,w,D.deleteL i d,r))

dispLookup1 :: Int -> Rez v (Maybe Double)
dispLookup1 i = gets (\(c,x,y,z,w,d,r) -> D.lookupL1 i d)

setRunnable :: Int -> Rez v ()
setRunnable i = modify (\(c,x,y,z,w,d,r) -> (c,x,y,z,w,d,HS.insert i r))

clearRunnable :: Int -> Rez v ()
clearRunnable i = modify (\(c,x,y,z,w,d,r) -> (c,x,y,z,w,d,HS.delete i r))

checkRunnable :: Int -> Rez v Bool
checkRunnable i = gets (\(c,x,y,z,w,d,r) -> HS.member i r)

getCurrentTime :: Rez v Double
getCurrentTime = asks (\(x,y,z) -> x)

getOriginalProcs :: Rez v ProcTab
getOriginalProcs = asks (\(x,y,z) -> z)

forEachProcess ::
  (forall a . Pid a -> Process a -> Rez v b) -> Rez v [b]
forEachProcess f = do
  ps <- getCurrentProcs
  forM (HM.toList ps) (\(i, HideProc pid p) -> f pid p)

---
--
--

answer :: Pid a -> Process a -> W b -> W b
answer pid@(Pid i) pr w = w' where
  w' = w { wproctab = pt', wrunlist = r' }
  r' = HS.insert i (wrunlist w)
  pt' = overwriteProc pid pr (wproctab w)

requestThread ::
  Pid b -> IO a -> (a -> Process b) -> ((W v -> W v) -> IO ()) -> IO ()
requestThread pid io handler doReply = do
  x <- io
  let p' = handler x
  doReply (answer pid p')

