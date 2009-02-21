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

module IndependentScreens where

marshall (S sc) ws = show sc ++ '_':ws
unmarshall         = ((S . read) *** drop 1) . break (=='_')
workspaces'        = nub . map (snd . unmarshall) . workspaces
withScreens n workspaces = [marshall sc ws | ws <- workspaces, sc <- [0..n-1]]
onScreen    f workspace  = screen . current >>= f . flip marshall workspace
countScreens = fmap genericLength $ openDisplay "" >>= getScreenInfo
