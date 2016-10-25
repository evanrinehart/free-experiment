{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
module Process where 

import Data.HashMap.Strict (HashMap); import qualified Data.HashMap.Strict as HM
import Unsafe.Coerce
import Data.Function

import View
import Script

data Process sig x = forall s a . Proc
  { procScr   :: ScriptSG sig s x a
  , procGuts  :: Guts x
  , procState :: s }

data HideProc sig = forall a . HideProc (Pid a) (Process sig a)

type ProcTab sig = HashMap Int (HideProc sig)

-- warning!
replaceGuts :: Guts a -> Process sig a -> Process sig a
replaceGuts g@(ViewGuts _)  (Proc sc (ViewGuts _) st) = Proc sc g st
replaceGuts g@(GenGuts _ _) (Proc sc (GenGuts _ _) st)  = Proc sc g st
replaceGuts _ _ = error "replaceGuts failed"

-- warning!
--modifyGuts :: (a -> a) -> Guts a -> Guts a
--modifyGuts f (GenGuts g x) = GenGuts g (f x)
--modifyGuts _ _ = error "modifyGuts failed"

lookAtGuts :: Guts a -> Processes -> a
lookAtGuts (ViewGuts v) ps = runView v ps
lookAtGuts (GenGuts _ x) ps = x

lookupProc :: HashMap Int (HideProc sig) -> Pid a -> Maybe (Process sig a)
lookupProc hm (Pid i) = case HM.lookup i hm of
  Just (HideProc _ p) -> Just (unsafeCoerce p)
  Nothing -> Nothing

-- I coded myself into a situation where I need to use a fixed point.
-- to look at a view, I need to look at processes
-- to look at processes, I need to look at views.
-- everything works as long as everything is based on generators (no cycles)
viewEquation :: ProcTab sig -> Processes -> Processes
viewEquation tab ps = Procs f where
  f :: Pid a -> Maybe a
  f pid = case lookupProc tab pid of
    Nothing -> Nothing
    Just p -> Just (lookAtGuts (procGuts p) ps)

-- tie the knot
viewFixedPoint :: ProcTab sig -> Processes
viewFixedPoint tab = fix (viewEquation tab)

insertProc ::
  Process sig a ->
  Int ->
  ProcTab sig ->
  (Int, Pid a, ProcTab sig)
insertProc p counter hm = (counter+1, pid, hm') where
  pid = Pid counter
  hm' = HM.insert counter (HideProc pid p) hm

overwriteProc ::
  Pid a ->
  Process sig a ->
  ProcTab sig ->
  ProcTab sig
overwriteProc pid@(Pid i) pr' hm = HM.insert i (HideProc pid pr') hm

eachProc :: ProcTab sig -> (forall a . Pid a -> Process sig a -> b) -> [b]
eachProc ps f = map g (HM.toList ps) where
  g (_,HideProc pid p) = f pid p 

procTabFromList :: [(Int, HideProc sig)] -> ProcTab sig
procTabFromList = HM.fromList
