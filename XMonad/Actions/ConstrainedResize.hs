-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Actions.ConstrainedResize
-- Description :  Constrain the aspect ratio of a floating window.
-- Copyright   :  (c) Dougal Stanton
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  <dougal@dougalstanton.net>
-- Stability   :  stable
-- Portability :  unportable
--
-- Lets you constrain the aspect ratio of a floating
-- window (by, say, holding shift while you resize).
--
-- Useful for making a nice circular XClock window.
--
-----------------------------------------------------------------------------

module XMonad.Actions.ConstrainedResize (
        -- * Usage
        -- $usage
        XMonad.Actions.ConstrainedResize.mouseResizeWindow
) where

import XMonad

-- $usage
--
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import qualified XMonad.Actions.ConstrainedResize as Sqr
--
-- Then add something like the following to your mouse bindings:
--
-- >     , ((modm, button3),               (\w -> focus w >> Sqr.mouseResizeWindow w False))
-- >     , ((modm .|. shiftMask, button3), (\w -> focus w >> Sqr.mouseResizeWindow w True ))
--
-- The line without the shiftMask replaces the standard mouse resize
-- function call, so it's not completely necessary but seems neater
-- this way.
--
-- For detailed instructions on editing your mouse bindings, see
-- "XMonad.Doc.Extending#Editing_mouse_bindings".

-- | Resize (floating) window with optional aspect ratio constraints.
mouseResizeWindow :: Window -> Bool -> X ()
mouseResizeWindow w c = whenX (isClient w) $ withDisplay $ \d ->
  withWindowAttributes d w $ \wa -> do
    sh <- io $ getWMNormalHints d w
    io $ warpPointer d none w 0 0 0 0 (fromIntegral (wa_width wa)) (fromIntegral (wa_height wa))
    mouseDrag (\ex ey -> do
                 let x = ex - fromIntegral (wa_x wa)
                     y = ey - fromIntegral (wa_y wa)
                     sz = if c then (max x y, max x y) else (x,y)
                 io $ resizeWindow d w `uncurry`
                    applySizeHintsContents sh sz
                 float w)
              (float w)
