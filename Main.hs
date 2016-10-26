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

occ :: Port a -> a -> Occurrences
occ k v = singletonOcc k v

data Runtime = Rt (MVar (DriverAction Picture)) (MVar Picture)

glshow :: Runtime -> IO Picture
glshow (Rt mvIn mvOut) = do
  putMVar mvIn RenderDump
  takeMVar mvOut

glhandle :: Port () -> Port () -> Port Ctrl -> Event -> Runtime -> IO Runtime
glhandle p1 p2 p3 e rt@(Rt mvIn _) = go >> return rt where
  go = case e of
    EventKey (Char 'a') Down _ _ -> putMVar mvIn (Stimulus $ occ p3 CtrlLeft)
    EventKey (Char 's') Down _ _ -> putMVar mvIn (Stimulus $ occ p3 CtrlDown)
    EventKey (Char 'd') Down _ _ -> putMVar mvIn (Stimulus $ occ p3 CtrlRight)
    EventKey (Char 'w') Down _ _ -> putMVar mvIn (Stimulus $ occ p3 CtrlUp)
    EventKey (Char 'c') Down _ _ -> putMVar mvIn (Stimulus $ occ p2 ())
    EventKey (SpecialKey KeyEnter) Down _ _ -> putMVar mvIn (Stimulus $ occ p1 ())
    _ -> return ()

glstep :: Float -> Runtime -> IO Runtime
glstep dt rt@(Rt mvIn mvOut) = do
  putMVar mvIn (TimePass (realToFrac dt))
  return rt

startCore :: W Picture -> Kahan -> Runtime -> IO ()
startCore w k rt@(Rt mvIn mvOut) = do
  let (w', out) = resolve 0 mvIn (Occs (const [])) w
  out
  core w' k rt

core :: W Picture -> Kahan -> Runtime -> IO ()
core w k rt@(Rt mvIn mvOut) = do
  r <- takeMVar mvIn
  (w',k') <- case r of
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
        return (w',k')
    Stimulus occs@(Occs f) -> do
      let now = K.extract k
      let (w', out) = resolve now mvIn occs w
      out
      return (w',k)
    RenderDump -> do
      case viewRoot w of
        Just pic -> do
          putMVar mvOut pic
          return (w,k)
        Nothing -> do
          putStrLn "i see nothing"
          exitSuccess
    Answer f -> do
      let w' = f w
      let now = K.extract k
      let (w'',out) = resolve now mvIn (Occs (const [])) w'
      out
      return (w'',k)
  when (rootIsGone w') $ do
    putStrLn ("["++show (extract k)++","++show (extract k')++"]: *poof*")
    exitSuccess
  core w' k' rt

main = do
  mvIn <- newEmptyMVar
  rt <- Rt mvIn <$> newEmptyMVar
  let dm = InWindow "Pacman" (640,480) (0,0)
  (p1,_) <- newExternalPort
  (p2,coin) <- newExternalPort
  (p3,_) <- newExternalPort
  let w = setupW blank () (program coin)
  forkIO $ do
    playIO dm white 200 rt glshow (glhandle p1 p2 p3) glstep 
  startCore w K.zero rt
