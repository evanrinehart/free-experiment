{-# LANGUAGE GADTs #-}
module Pacman where

import Graphics.Gloss
import Script
import Signal
import Event

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

debug :: String -> Script l s v ()
debug = exec . putStrLn

program :: Script Signal () Picture a
program = do
  debug "fooooooo"
  sleep 1
  setView (pure (rectangleSolid 100 100))
  debug "baaaaaar"
  sleep 5
  await (onSignal SigCoin)
  terminate

-- look at a collection of things, if some of them are not there anymore
-- then forget their view
--viewMaybes ::
--  [View (Maybe a)] -> ([a] -> b) -> Script sig c (b, [View (Maybe)])
