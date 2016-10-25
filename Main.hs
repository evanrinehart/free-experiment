module Main where

import Graphics.Gloss.Interface.IO.Game
import Control.Concurrent
import Control.Concurrent.MVar
import System.Exit
import Data.Function
import Control.Exception
import Control.Monad

import Data.HashMap.Strict as HM

import Signal
import Runner
import Script
import Pacman
import Algorithm
import Kahan as K
import Process

occ :: Keys sig => sig a -> a -> Occurrences sig
occ k v = singletonOcc k v

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
  core w' k rt

core :: W Signal Picture -> Kahan -> Runtime -> IO ()
core w k rt@(Rt mvIn mvOut) = do
  r <- takeMVar mvIn
  case r of
    TimePass dt ->
      let k'   = K.accum k dt in
      let now  = K.extract k in
      let now' = K.extract k' in
      do
        w' <- fix (\loop w t -> do
          case advanceFromTo t now' mvIn w of
            Left w' -> do
              return w'
            Right (t', w', out) -> do
              out
              loop w' t') w now
        when (rootIsGone w') $ do
          putStrLn ("["++show (extract k)++","++show (extract k')++"]: *poof*")
          exitSuccess
        core w' k' rt
    Stimulus occs@(Occs f) -> do
      let s0 = SigN (toNumber SigCoin) :: SigN Signal ()
      let s1 = SigN (toNumber Sig1P) :: SigN Signal ()
      let s2 = SigN (toNumber SigControl) :: SigN Signal Ctrl
      print (f s0, f s1, f s2)
      let now = K.extract k
      let (w', out) = resolve now mvIn occs w
      out
      when (rootIsGone w') $ do
        putStrLn ("["++show (extract k)++","++show (extract k)++"]: *poof*")
        exitSuccess
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
      print "not happening"
      core (f w) k rt

main = do
  mvIn <- newEmptyMVar
  rt <- Rt mvIn <$> newEmptyMVar
  let w = setupW blank () program
  let dm = InWindow "Pacman" (640,480) (0,0)
  forkIO $ do
    playIO dm white 100 rt glshow glhandle glstep 
  startCore w K.zero rt
