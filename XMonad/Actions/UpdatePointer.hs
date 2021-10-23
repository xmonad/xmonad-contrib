{-# LANGUAGE ScopedTypeVariables #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  XMonadContrib.UpdatePointer
-- Description :  Causes the pointer to follow whichever window focus changes to.
-- Copyright   :  (c) Robert Marlow <robreim@bobturf.org>, 2015 Evgeny Kurnevsky
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
    )
    where

import XMonad
import XMonad.Prelude
import XMonad.StackSet (member, peek, screenDetail, current)

import Control.Arrow ((&&&), (***))

-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad
-- > import XMonad.Actions.UpdatePointer
--
-- Enable it by including it in your logHook definition, e.g.:
--
-- > logHook = updatePointer (0.5, 0.5) (1, 1)
--
-- which will move the pointer to the nearest point of a newly focused
-- window. The first argument establishes a reference point within the
-- newly-focused window, while the second argument linearly interpolates
-- between said reference point and the edges of the newly-focused window to
-- obtain a bounding box for the pointer.
--
-- > logHook = updatePointer (0.5, 0.5) (0, 0) -- exact centre of window
-- > logHook = updatePointer (0.25, 0.25) (0.25, 0.25) -- near the top-left
-- > logHook = updatePointer (0.5, 0.5) (1.1, 1.1) -- within 110% of the edge
--
-- To use this with an existing logHook, use >> :
--
-- > logHook = dynamicLog
-- >           >> updatePointer (1, 1) (0, 0)
--
-- which moves the pointer to the bottom-right corner of the focused window.

-- | Update the pointer's location to the currently focused
-- window or empty screen unless it's already there, or unless the user was changing
-- focus with the mouse
--
-- See also 'XMonad.Actions.UpdateFocus.focusUnderPointer' for an inverse
-- operation that updates the focus instead. The two can be combined in a
-- single config if neither goes into 'logHook' but are invoked explicitly in
-- individual key bindings.
updatePointer :: (Rational, Rational) -> (Rational, Rational) -> X ()
updatePointer refPos ratio = do
  ws <- gets windowset
  dpy <- asks display
  let defaultRect = screenRect $ screenDetail $ current ws
  rect <- case peek ws of
        Nothing -> return defaultRect
        Just w  -> maybe defaultRect windowAttributesToRectangle
               <$> safeGetWindowAttributes w

  root <- asks theRoot
  mouseIsMoving <- asks mouseFocused
  (_sameRoot,_,currentWindow,rootX,rootY,_,_,_) <- io $ queryPointer dpy root
  drag <- gets dragging
  unless (pointWithin (fi rootX) (fi rootY) rect
          || mouseIsMoving
          || isJust drag
          || not (currentWindow `member` ws || currentWindow == none)) $ let
    -- focused rectangle
    (rectX, rectY) = (rect_x &&& rect_y) rect
    (rectW, rectH) = (fi . rect_width &&& fi . rect_height) rect
    -- reference position, with (0,0) and (1,1) being top-left and bottom-right
    refX = lerp (fst refPos) rectX (rectX + rectW)
    refY = lerp (snd refPos) rectY (rectY + rectH)
    -- final pointer bounds, lerped *outwards* from reference position
    boundsX = join (***) (lerp (fst ratio) refX) (rectX, rectX + rectW)
    boundsY = join (***) (lerp (snd ratio) refY) (rectY, rectY + rectH)
    -- ideally we ought to move the pointer in a straight line towards the
    -- reference point until it is within the above bounds, but…
    in io $ warpPointer dpy none root 0 0 0 0
        (round . clip boundsX $ fi rootX)
        (round . clip boundsY $ fi rootY)

windowAttributesToRectangle :: WindowAttributes -> Rectangle
windowAttributesToRectangle wa = Rectangle (fi (wa_x wa))
                                           (fi (wa_y wa))
                                           (fi (wa_width wa + 2 * wa_border_width wa))
                                           (fi (wa_height wa + 2 * wa_border_width wa))

lerp :: (RealFrac r, Real a, Real b) => r -> a -> b -> r
lerp r a b = (1 - r) * realToFrac a + r * realToFrac b

clip :: Ord a => (a, a) -> a -> a
clip (lower, upper) x
  | x < lower = lower
  | x > upper = upper
  | otherwise = x
