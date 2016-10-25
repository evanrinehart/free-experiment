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

data Signal a where
  SigControl :: Signal Ctrl
  Sig1P      :: Signal ()
  SigCoin    :: Signal ()
  -- Collision
  -- ScreenChange

instance Keys Signal where
  toNumber s = case s of
    SigControl -> 0
    Sig1P      -> 1
    SigCoin    -> 2

type ScriptS s v a = ScriptSG Signal s v a
type Script v a = ScriptS () v a

debug :: String -> Script v ()
debug = exec . putStrLn

ctrl = onSignal SigControl
onL = fmapMaybeE (\case CtrlLeft  -> Just (); _ -> Nothing) ctrl
onR = fmapMaybeE (\case CtrlRight -> Just (); _ -> Nothing) ctrl
hmm = onL <> onR <> mempty

spinny :: Double -> Picture -> Picture
spinny t pic = rotate (realToFrac (t*50)) pic

program :: Script Picture a
program = do
  let v1 = pure (rectangleSolid 100 100)
  setView v1 -- show black rectangle
  (e,v) <- fork () (ViewGuts (pure blank)) pr2
  x <- await e
  exec (print x)
  x <- await e
  exec (print x)
  sleep 5
  terminate
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

-- look at a collection of things, if some of them are not there anymore
-- then forget their view
--viewMaybes ::
--  [View (Maybe a)] -> ([a] -> b) -> Script sig c (b, [View (Maybe)])
