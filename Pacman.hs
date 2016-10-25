{-# LANGUAGE GADTs #-}
module Pacman where

import Graphics.Gloss
import Script
import Signal

data Ctrl = CtrlLeft | CtrlRight | CtrlUp | CtrlDown

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

program :: Script Signal () Picture a
program = do
  exec (putStrLn "fooooo")
  sleep 1
  exec (putStrLn "bbbbar")
  sleep 5
  terminate

-- look at a collection of things, if some of them are not there anymore
-- then forget their view
--viewMaybes ::
--  [View (Maybe a)] -> ([a] -> b) -> Script sig c (b, [View (Maybe)])
