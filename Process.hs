{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
module Process where 

import Data.HashMap.Strict (HashMap); import qualified Data.HashMap.Strict as HM
import Unsafe.Coerce
import Data.Function

import View
import Script

data Process x = forall s a . Proc
  { procScr   :: Script x a
  , procGuts  :: Guts x
  }

data HideProc = forall a . HideProc (Pid a) (Process a)
type ProcTab = HashMap Int HideProc

-- warning!
--replaceGuts :: Guts a -> Process a -> Process a
--replaceGuts g@(ViewGuts _) (Proc sc (ViewGuts _) st) = Proc sc g st
--replaceGuts g@(GenGuts _ _) (Proc sc (GenGuts _ _) st)  = Proc sc g st
--replaceGuts _ _ = error "replaceGuts failed"

-- warning!
--modifyGuts :: (a -> a) -> Guts a -> Guts a
--modifyGuts f (GenGuts g x) = GenGuts g (f x)
--modifyGuts _ _ = error "modifyGuts failed"

lookAtGuts :: Guts a -> Processes -> a
lookAtGuts (Guts x _) _ = x

lookupProc :: HashMap Int HideProc -> Pid a -> Maybe (Process a)
lookupProc hm (Pid i) = case HM.lookup i hm of
  Just (HideProc _ p) -> Just (unsafeCoerce p)
  Nothing -> Nothing

-- I coded myself into a situation where I need to use a fixed point.
-- to look at a view, I need to look at processes
-- to look at processes, I need to look at views.
-- everything works as long as everything is based on generators (no cycles)
viewEquation :: ProcTab -> Processes -> Processes
viewEquation tab ps = Procs f where
  f :: Pid a -> Maybe a
  f pid = case lookupProc tab pid of
    Nothing -> Nothing
    Just p -> Just (lookAtGuts (procGuts p) ps)

-- tie the knot
viewFixedPoint :: ProcTab -> Processes
viewFixedPoint tab = fix (viewEquation tab)

insertProc ::
  Process a ->
  Int ->
  ProcTab ->
  (Int, Pid a, ProcTab)
insertProc p counter hm = (counter+1, pid, hm') where
  pid = Pid counter
  hm' = HM.insert counter (HideProc pid p) hm

overwriteProc ::
  Pid a ->
  Process a ->
  ProcTab ->
  ProcTab
overwriteProc pid@(Pid i) pr' hm = HM.insert i (HideProc pid pr') hm

eachProc :: ProcTab -> (forall a . Pid a -> Process a -> b) -> [b]
eachProc ps f = map g (HM.toList ps) where
  g (_,HideProc pid p) = f pid p 

procTabFromList :: [(Int, HideProc)] -> ProcTab
procTabFromList = HM.fromList
