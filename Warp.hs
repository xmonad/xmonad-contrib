-----------------------------------------------------------------------------
-- |
-- Module      :  XMonadContrib.Warp
-- Copyright   :  (c) daniel@wagner-home.com
-- License     :  BSD3-style (see LICENSE)
-- 
-- Maintainer  :  daniel@wagner-home.com
-- Stability   :  unstable
-- Portability :  unportable
--
-- This can be used to make a keybinding that warps the pointer to a given
-- window or screen.
--
-----------------------------------------------------------------------------

module XMonadContrib.Warp (
                           -- * Usage
                           -- $usage
                           warpToScreen, 
                           warpToWindow
                          ) where

import Data.Ratio
import Data.List
import Control.Monad.RWS
import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras
import XMonad.Operations
import XMonad
import XMonad.StackSet as W

{- $usage
This can be used to make a keybinding that warps the pointer to a given
window or screen.  For example, I've added the following keybindings to
my Config.hs:

> , ((modMask,   xK_z     ), warpToWindow (1%2) (1%2)) -- @@ Move pointer to currently focused window
> 
>-- mod-ctrl-{w,e,r} @@ Move mouse pointer to screen 1, 2, or 3
>   
>   [((modMask .|. controlMask, key), warpToScreen sc (1%2) (1%2))
>       | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]]

Note that warping to a particular screen may change the focus.
-}

-- %import XMonadContrib.Warp
-- %keybind , ((modMask,   xK_z     ), warpToWindow (1%2) (1%2)) -- @@ Move pointer to currently focused window
-- %keybindlist ++
-- %keybindlist -- mod-ctrl-{w,e,r} @@ Move mouse pointer to screen 1, 2, or 3
-- %keybindlist [((modMask .|. controlMask, key), warpToScreen sc (1%2) (1%2))
-- %keybindlist     | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]]

fraction :: (Integral a, Integral b) => Rational -> a -> b
fraction f x = floor (f * fromIntegral x)

warp :: Window -> Position -> Position -> X ()
warp w x y = withDisplay $ \d -> io $ warpPointer d none w 0 0 0 0 x y

warpToWindow :: Rational -> Rational -> X ()
warpToWindow h v =
    withDisplay $ \d ->
        withFocused $ \w -> do
            wa <- io $ getWindowAttributes d w
            warp w (fraction h (wa_width wa)) (fraction v (wa_height wa))

warpToScreen :: ScreenId -> Rational -> Rational -> X ()
warpToScreen n h v = do
    root <- asks theRoot
    (StackSet {current = x, visible = xs}) <- gets windowset
    whenJust (fmap (screenRect . W.screenDetail) . find ((n==) . W.screen) $ x : xs)
        $ \r ->
            warp root (rect_x r + fraction h (rect_width  r))
                      (rect_y r + fraction v (rect_height r))
