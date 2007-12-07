-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Actions.Warp
-- Copyright   :  (c) daniel@wagner-home.com
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  daniel@wagner-home.com
-- Stability   :  unstable
-- Portability :  unportable
--
-- Warp the pointer to a given window or screen.
--
-----------------------------------------------------------------------------

module XMonad.Actions.Warp (
                           -- * Usage
                           -- $usage
                           warpToScreen,
                           warpToWindow
                          ) where

import Data.Ratio
import Data.List
import XMonad.Operations
import XMonad
import XMonad.StackSet as W

{- $usage
You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:

> import XMonad.Actions.Warp

then add appropriate keybindings to warp the pointer; for example:

> , ((modMask x,   xK_z     ), warpToWindow (1%2) (1%2)) -- @@ Move pointer to currently focused window
>
>-- mod-ctrl-{w,e,r} @@ Move mouse pointer to screen 1, 2, or 3
>
>   [((modMask x .|. controlMask, key), warpToScreen sc (1%2) (1%2))
>       | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]]

Note that warping to a particular screen may change the focus.
-}

-- For detailed instructions on editing your key bindings, see
-- "XMonad.Doc.Extending#Editing_key_bindings".

fraction :: (Integral a, Integral b) => Rational -> a -> b
fraction f x = floor (f * fromIntegral x)

warp :: Window -> Position -> Position -> X ()
warp w x y = withDisplay $ \d -> io $ warpPointer d none w 0 0 0 0 x y

-- | Warp the pointer to a given position relative to the currently
--   focused window.  Top left = (0,0), bottom right = (1,1).
warpToWindow :: Rational -> Rational -> X ()
warpToWindow h v =
    withDisplay $ \d ->
        withFocused $ \w -> do
            wa <- io $ getWindowAttributes d w
            warp w (fraction h (wa_width wa)) (fraction v (wa_height wa))

-- | Warp the pointer to the given position (top left = (0,0), bottom
--   right = (1,1)) on the given screen.
warpToScreen :: ScreenId -> Rational -> Rational -> X ()
warpToScreen n h v = do
    root <- asks theRoot
    (StackSet {current = x, visible = xs}) <- gets windowset
    whenJust (fmap (screenRect . W.screenDetail) . find ((n==) . W.screen) $ x : xs)
        $ \r ->
            warp root (rect_x r + fraction h (rect_width  r))
                      (rect_y r + fraction v (rect_height r))
