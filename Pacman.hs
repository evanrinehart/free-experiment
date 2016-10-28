{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
module Pacman where

import Data.Monoid
import Data.Maybe
import Graphics.Gloss
import Control.Concurrent

import Script
import Signal
import Event
import View

data Ctrl = CtrlLeft | CtrlRight | CtrlUp | CtrlDown
  deriving (Show)

debug :: String -> Script v ()
debug = exec . putStrLn

program :: Event () -> Script Picture a
program coin = do
  await coin
  (p,e) <- newPort
  fork (Guts () (const id)) (satellite p)
  await e
  terminate

satellite :: Port Int -> Script () a
satellite out = do
  sleep 2
  trigger out 9
  hang

{-
ctrl = onSignal SigControl
onL = fmapMaybeE (\case CtrlLeft  -> Just (); _ -> Nothing) ctrl
onR = fmapMaybeE (\case CtrlRight -> Just (); _ -> Nothing) ctrl
hmm = onL <> onR <> mempty

spinny :: Double -> Picture -> Picture
spinny t pic = rotate (realToFrac (t*50)) pic

type EntTempl a b = (Guts a, Script a b)
type ScriptV a = ScriptS (View Picture) Picture a

spawn :: EntTempl Picture b -> ScriptV (Event ())
spawn (guts,scr) = do
  accum <- get
  (e, v) <- fork () guts scr
  let accum' = accum <> fmap (fromMaybe blank) v
  setView accum'
  put accum'
  return (fmap (const ()) e)

cue :: Ent Picture b -> Bool -> String -> ScriptV ()
cue ent temp msg = do
  e <- spawn ent
  spawn (banner (runTemp e never temp) msg)
  await (eitherE onStartGame e) >>= \case 
    Left _  -> terminate -- game is starting
    Right _ -> return ()

data Temp = Temp | Stay
runTemp :: a -> a -> Temp -> a
runTemp x y Temp = x
runTemp x y Stay = y

titleScreen :: ScriptV ()
titleScreen = do
  ready <- spawn bannerBox
  await ready
  cue (ghost 1) Temp "starring red ghost"
  cue (ghost 2) Temp "pink ghost"
  cue (ghost 3) Temp "cyan ghost"
  cue (ghost 4) Temp "orange ghost"
  cue mspacman  Stay "starring ms pacman"
  sleep 10
  -- title screen ends

program :: Script Picture a
program = do
  let v1 = pure (rectangleSolid 100 100)
  setView v1 -- show black rectangle
  await hmm -- test event combining
  clk <- fmap (fmap fromJust . snd) (fork () (GenGuts (+) 0) (await never)) -- test generator
  v2 <- fmap snd $ fork () (ViewGuts $ spinny <$> clk <*> pure (translate 200 200 $ rectangleSolid 100 100)) pr2
  let v3 = fmap (fromMaybe blank) v2 
  setView (v1 <> v3)
  debug "baaaaaar"
  c <- request $ do
    print "requesting"
    threadDelay 3000000
    return 'j'
  debug (show c)
  sleep 8
  terminate

pr2 :: Script Picture a
pr2 = do
  debug "pr2 start"
  sleep 3
  debug "wokeup"
  checkpoint
  checkpoint
  checkpoint
  debug "checkpointed"
  sleep 3
  debug "terminating"
  terminate
-}

-- look at a collection of things, if some of them are not there anymore
-- then forget their view
--viewMaybes ::
--  [View (Maybe a)] -> ([a] -> b) -> Script sig c (b, [View (Maybe)])
