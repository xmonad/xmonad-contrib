-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Actions.Warp
-- Description :  Warp the pointer to a given window or screen.
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
                           banish,
                           banishScreen,
                           Corner(..),
                           warpToScreen,
                           warpToWindow
                          ) where

import XMonad.Prelude
import XMonad
import XMonad.StackSet as W

{- $usage
You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:

> import XMonad.Actions.Warp

then add appropriate keybindings to warp the pointer; for example:

> , ((modm,   xK_z     ), warpToWindow (1%2) (1%2)) -- @@ Move pointer to currently focused window
>
>-- mod-ctrl-{w,e,r} @@ Move mouse pointer to screen 1, 2, or 3
>
>   [((modm .|. controlMask, key), warpToScreen sc (1%2) (1%2))
>       | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]]

Note that warping to a particular screen may change the focus.
-}

-- For detailed instructions on editing your key bindings, see
-- "XMonad.Doc.Extending#Editing_key_bindings".


data Corner = UpperLeft | UpperRight | LowerLeft | LowerRight

{- | Move the mouse cursor to a corner of the focused window. Useful for
   uncluttering things.

   Internally, this uses numerical parameters. We parametrize on the 'Corner'
   type so the user need not see the violence inherent in
   the system.

   'warpToScreen' and 'warpToWindow' can be used in a variety of
   ways. Suppose you wanted to emulate Ratpoison's \'banish\' command,
   which moves the mouse pointer to a corner? warpToWindow can do that! -}
banish :: Corner -> X ()
banish direction = case direction of
                     LowerRight -> warpToWindow 1 1
                     LowerLeft  -> warpToWindow 0 1
                     UpperLeft  -> warpToWindow 0 0
                     UpperRight -> warpToWindow 1 0

{- | Same as 'banish' but moves the mouse to the corner of the
   currently focused screen -}
banishScreen :: Corner -> X ()
banishScreen direction = case direction of
                           LowerRight -> warpToCurrentScreen 1 1
                           LowerLeft  -> warpToCurrentScreen 0 1
                           UpperLeft  -> warpToCurrentScreen 0 0
                           UpperRight -> warpToCurrentScreen 1 0
    where
      warpToCurrentScreen h v =
          do ws <- gets windowset
             warpToScreen (W.screen $ current ws) h v
             windows (const ws)


fraction :: (Integral a, Integral b) => Rational -> a -> b
fraction f x = floor (f * fromIntegral x)

warp :: Window -> Position -> Position -> X ()
warp w x y = withDisplay $ \d -> io $ warpPointer d none w 0 0 0 0 x y

-- | Warp the pointer to a given position relative to the currently
--   focused window.  Top left = (0,0), bottom right = (1,1).
warpToWindow :: Rational -> Rational -> X ()
warpToWindow h v = withDisplay $ \d -> withFocused $ \w ->
  withWindowAttributes d w $ \wa ->
    warp w (fraction h (wa_width wa)) (fraction v (wa_height wa))

-- | Warp the pointer to the given position (top left = (0,0), bottom
--   right = (1,1)) on the given screen.
warpToScreen :: ScreenId -> Rational -> Rational -> X ()
warpToScreen n h v = do
    root <- asks theRoot
    StackSet{current = x, visible = xs} <- gets windowset
    whenJust (fmap (screenRect . W.screenDetail) . find ((n==) . W.screen) $ x : xs)
        $ \r ->
            warp root (rect_x r + fraction h (rect_width  r))
                      (rect_y r + fraction v (rect_height r))
