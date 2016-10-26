module Runner where

import Control.Concurrent.MVar
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS

import View
import Signal
import Script
import Process
import Kahan as K
import Relation (Relation); import qualified Relation as D
import Algorithm
import UUID
import Event

-- when the root is gone, the program must end
rootIsGone :: W a -> Bool
rootIsGone w = not (HM.member (unPid (wroot w)) (wproctab w))

--- assume nothing stopping us from advancing this far
advanceRaw :: Double -> W a -> W a
advanceRaw dt w = w { wproctab = tab' } where
  tab = wproctab w
  ps = viewFixedPoint tab
  tab' = HM.mapWithKey f tab
  f i orig@(HideProc pid (Proc scr guts st)) = case guts of
    GenGuts g x -> HideProc pid (Proc scr (GenGuts g (g dt x)) st)
    _ -> orig

advanceFromTo
  :: Double
  -> Double
  -> MVar (DriverAction a)
  -> W a
  -> Either (W a) (Double, W a, IO ())
advanceFromTo t t' mv w =
  let t'' = nextBreakAt w in
  if t' < t''
    then Left (advanceRaw (t' - t) w)
    else
      let w' = advanceRaw (t'' - t) w in
      let (w'', out) = resolve t'' mv (Occs (const [])) w in
      Right (t'', w'', out)

-- given the current absolute time, compute time until next break.
-- if there is no such time returns floating point infinity
nextBreakAt :: W a -> Double
nextBreakAt w = case D.minR (wdisp w) of
  Just (t,_) -> t
  Nothing -> 1 / 0

setupW :: v -> s -> ScriptS s v a -> W v
setupW blank st0 prog =
  let tab0 = procTabFromList [] in
  let p0 = Proc prog (ViewGuts (pure blank)) st0 in
  let (c, pid0, tab0') = insertProc p0 0 tab0 in
  W c pid0 tab0' D.empty (HS.fromList [0])

viewRoot :: W a -> Maybe a
viewRoot w =
  let tab = wproctab w in
  fmap (\x -> lookAtGuts x (viewFixedPoint tab)) (fmap procGuts $ lookupProc tab (wroot w))

requestThread ::
  Pid b -> IO a -> (a -> Process b) -> ((W v -> W v) -> IO ()) -> IO ()
requestThread pid io handler doReply = do
  x <- io
  let p' = handler x
  doReply (answer pid p')

newExternalPort :: IO (Port a, Event a)
newExternalPort = do
  uuid <- newUUID
  let p = ExtPort uuid
  return (p, onPort p)

-- what we need for gloss
-- World sig Picture
-- stim :: G.Event -> W sig Picture -> IO (W sig Picture)
-- pass :: Float -> W sig Picture -> IO (W sig Picture)
