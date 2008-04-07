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
-- keep the mouse near the currently focused window
--
-----------------------------------------------------------------------------

module XMonad.Actions.UpdatePointer 
    (
     -- * Usage
     -- $usage
     updatePointer
     , PointerPosition (..)
    )
    where

import XMonad
import Control.Monad

-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad
-- > import XMonad.Actions.UpdatePointer
--
-- Enable it by including it in your logHook definition. Eg:
-- 
-- > logHook = updatePointer Nearest
-- 
-- which will move the pointer to the nearest point of a newly focused window, or
--
-- > logHook = updatePointer (Relative 0.5 0.5)
--
-- which will move the pointer to the center of a newly focused window.
--
-- To use this with an existing logHook, use >> :
--
-- > logHook = dynamicLog
-- >           >> updatePointer (Relative 1 1)
--
-- which moves the pointer to the bottom-right corner of the focused window.

data PointerPosition = Nearest | Relative Rational Rational

-- | Update the pointer's location to the currently focused
-- window unless it's already there
updatePointer :: PointerPosition -> X ()
updatePointer p = withFocused $ \w -> do
  dpy <- asks display
  root <- asks theRoot
  wa <- io $ getWindowAttributes dpy w
  (_sameRoot,_,w',rootx,rooty,_,_,_) <- io $ queryPointer dpy root
  -- Can sameRoot ever be false in this case? I'm going to assume not
  unless (w == w') $
    case p of
    Nearest -> do
      let x = moveWithin rootx (wa_x wa) ((wa_x wa) + (wa_width  wa))
      let y = moveWithin rooty (wa_y wa) ((wa_y wa) + (wa_height wa))
      io $ warpPointer dpy none root 0 0 0 0 (fromIntegral x) (fromIntegral y)
    Relative h v ->
      io $ warpPointer dpy none w 0 0 0 0
           (fraction h (wa_width wa)) (fraction v (wa_height wa))
        where fraction x y = floor (x * fromIntegral y)

moveWithin :: Integral a => a -> a -> a -> a
moveWithin current lower upper =
    if current < lower
    then lower
    else if current > upper
         then upper
         else current

