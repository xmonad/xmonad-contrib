-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.IndependentScreens
-- Copyright   :  (c) 2009 Daniel Wagner
-- License     :  BSD3
--
-- Maintainer  :  <daniel@wagner-home.com>
-- Stability   :  unstable
-- Portability :  unportable
--
-- Utility functions for simulating independent sets of workspaces on
-- each screen (like dwm's workspace model), using internal tags to
-- distinguish workspaces associated with each screen.
-----------------------------------------------------------------------------

module XMonad.Layout.IndependentScreens where

-- for the screen stuff
import Control.Arrow hiding ((|||))
import Control.Monad
import Control.Monad.Instances
import Data.List
import Graphics.X11.Xinerama
import XMonad
import XMonad.StackSet hiding (workspaces)

type VirtualWorkspace  = String
type PhysicalWorkspace = String

marshall :: ScreenId -> VirtualWorkspace -> PhysicalWorkspace
marshall (S sc) vws = show sc ++ '_':vws

unmarshall :: PhysicalWorkspace -> (ScreenId, VirtualWorkspace)
unmarshall = ((S . read) *** drop 1) . break (=='_')

workspaces' :: XConfig l -> [VirtualWorkspace]
workspaces' = nub . map (snd . unmarshall) . workspaces

withScreens :: ScreenId -> [VirtualWorkspace] -> [PhysicalWorkspace]
withScreens n vws = [marshall sc pws | pws <- vws, sc <- [0..n-1]]

onScreen :: (VirtualWorkspace -> WindowSet -> a) -> (PhysicalWorkspace -> WindowSet -> a)
onScreen f vws = screen . current >>= f . flip marshall vws

countScreens :: (MonadIO m, Integral i) => m i
countScreens = liftM genericLength . liftIO $ openDisplay "" >>= getScreenInfo
