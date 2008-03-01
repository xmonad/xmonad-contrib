-----------------------------------------------------------------------------
-- |
-- Module      :  XMonadContrib.UpdatePointer
-- Copyright   :  (c) Robert Marlow <robreim@bobturf.org>
-- License     :  BSD3-style (see LICENSE)
-- 
-- Maintainer  :  Robert Marlow <robreim@bobturf.org>
-- Stability   :  stable
-- Portability :  portable
--
-- Causes the pointer to follow whichever window focus changes to. Compliments
-- the idea of switching focus as the mouse crosses window boundaries to
-- keep the mouse near the currently focussed window
--
-----------------------------------------------------------------------------

module XMonad.Actions.UpdatePointer 
    (
     -- * Usage
     -- $usage
     updatePointer
    )
    where

import XMonad
import Control.Monad

-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad
-- > import XMonad.Hooks.DynamicLog
--
-- Enable it by including it in your logHook definition. Eg:
-- 
-- > logHook = updatePointer (1%2) (1%2)
-- 
-- which will move the pointer to the middle of a newly focused window if the
-- focus moves away from the pointer


-- | Update the pointer's location to the currently focused window unless it's 
-- already there
updatePointer ::  Rational -> Rational -> X ()
updatePointer h v = withFocused $ \w -> do
  dpy <- asks display
  root <- asks theRoot
  wa <- io $ getWindowAttributes dpy w
  (sameRoot,_,w',_,_,_,_,_) <- io $ queryPointer dpy root
  unless (sameRoot && w == w') $
         io $ warpPointer dpy none w 0 0 0 0
                (fraction h (wa_width wa)) (fraction v (wa_height wa))
    where fraction x y = floor (x * fromIntegral y)

