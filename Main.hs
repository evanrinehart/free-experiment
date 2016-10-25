module Main where

import Graphics.Gloss.Interface.IO.Game
import Control.Concurrent
import Control.Concurrent.MVar
import System.Exit
import Data.Function
import Control.Exception

import Data.HashMap.Strict as HM

import Signal
import Runner
import Script
import Pacman
import Algorithm
import Kahan as K

occ :: sig a -> a -> Occurrences sig
occ k v = Occs f where
  f _ = []

data Runtime = Rt (MVar (DriverAction Signal Picture)) (MVar Picture)

glshow :: Runtime -> IO Picture
glshow (Rt mvIn mvOut) = do
  putMVar mvIn RenderDump
  takeMVar mvOut

glhandle :: Event -> Runtime -> IO Runtime
glhandle e rt@(Rt mvIn _) = go >> return rt where
  go = case e of
    EventKey (Char 'a') Down _ _ -> putMVar mvIn (Stimulus $ occ SigControl CtrlLeft)
    EventKey (Char 's') Down _ _ -> putMVar mvIn (Stimulus $ occ SigControl CtrlDown)
    EventKey (Char 'd') Down _ _ -> putMVar mvIn (Stimulus $ occ SigControl CtrlRight)
    EventKey (Char 'w') Down _ _ -> putMVar mvIn (Stimulus $ occ SigControl CtrlUp)
    EventKey (Char 'c') Down _ _ -> putMVar mvIn (Stimulus $ occ SigCoin ())
    EventKey (SpecialKey KeyEnter) Down _ _ -> putMVar mvIn (Stimulus $ occ Sig1P ())
    _ -> return ()

glstep :: Float -> Runtime -> IO Runtime
glstep dt rt@(Rt mvIn mvOut) = do
  putMVar mvIn (TimePass (realToFrac dt))
  return rt

startCore :: W Signal Picture -> Kahan -> Runtime -> IO ()
startCore w k rt@(Rt mvIn mvOut) = do
  let (w', out) = resolve 0 mvIn (Occs (const [])) w
  out
  core w k rt

core :: W Signal Picture -> Kahan -> Runtime -> IO ()
core w k rt@(Rt mvIn mvOut) = do
--  putStrLn "core"
  r <- takeMVar mvIn
  case r of
    TimePass dt ->
      let k' = K.accum k dt in
      let now = K.extract k in
      let now' = K.extract k' in
      do
        w' <- fix (\loop w t -> do
          case advanceFromTo t now' mvIn w of
            Left w' -> return w'
            Right (t', w', out) -> do
              print "hmm"
              out
              loop w' t') w now
        core w' k' rt
    Stimulus occs -> do
      putStrLn "2"
      let now = K.extract k
      let (w', out) = resolve now mvIn occs w
      out
      core w' k rt
    RenderDump -> do
      case viewRoot w of
        Just pic -> do
          putMVar mvOut pic
          core w k rt
        Nothing -> do
          putStrLn "i see nothing"
          exitSuccess
    Answer f -> do
      putStrLn "answer"
      core (f w) k rt

main = do
  mvIn <- newEmptyMVar
  rt <- Rt mvIn <$> newEmptyMVar
  let w = setupW blank () program
  let dm = InWindow "Pacman" (640,480) (0,0)
  forkIO $ do
    playIO dm black 100 rt glshow glhandle glstep 
  startCore w K.zero rt
