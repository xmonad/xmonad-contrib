{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE ViewPatterns #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.LayoutScreens
-- Description :  A layout to divide a single screen into multiple screens.
-- Copyright   :  (c) David Roundy <droundy@darcs.net>
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  none
-- Stability   :  unstable
-- Portability :  unportable
--
-- Divide a single screen into multiple screens.
-----------------------------------------------------------------------------

module XMonad.Layout.LayoutScreens (
                                    -- * Usage
                                    -- $usage
                                    layoutScreens, layoutSplitScreen, fixedLayout,
                                    FixedLayout,
                                   ) where

import XMonad
import XMonad.Prelude
import qualified XMonad.StackSet as W

-- $usage
-- This module allows you to pretend that you have more than one screen by
-- dividing a single screen into multiple screens that xmonad will treat as
-- separate screens.  This should definitely be useful for testing the
-- behavior of xmonad under Xinerama, and it's possible that it'd also be
-- handy for use as an actual user interface, if you've got a very large
-- screen and long for greater flexibility (e.g. being able to see your
-- email window at all times, a crude mimic of sticky windows).
--
-- You can use this module with the following in your
-- @~\/.xmonad\/xmonad.hs@ file:
--
-- > import XMonad.Layout.LayoutScreens
-- > import XMonad.Layout.TwoPane
--
-- Then add some keybindings; for example:
--
-- >   , ((modm .|. shiftMask,                 xK_space), layoutScreens 2 (TwoPane 0.5 0.5))
-- >   , ((modm .|. controlMask .|. shiftMask, xK_space), rescreen)
--
-- Another example use would be to handle a scenario where xrandr didn't
-- work properly (e.g. a VNC X server in my case) and you want to be able
-- to resize your screen (e.g. to match the size of a remote VNC client):
--
-- > import XMonad.Layout.LayoutScreens
--
-- >   , ((modm .|. shiftMask, xK_space),
-- >        layoutScreens 1 (fixedLayout [Rectangle 0 0 1024 768]))
-- >   , ((modm .|. controlMask .|. shiftMask, xK_space), rescreen)
--
-- For detailed instructions on editing your key bindings, see
-- "XMonad.Doc.Extending#Editing_key_bindings".

-- | Modify all screens.
layoutScreens :: LayoutClass l Int => Int -> l Int -> X ()
layoutScreens nscr _ | nscr < 1 = trace $ "Can't layoutScreens with only " ++ show nscr ++ " screens."
layoutScreens nscr l = asks theRoot >>= \w -> withDisplay $ \d ->
  withWindowAttributes d w $ \attrs ->
    do let rtrect = windowRectangle attrs
       (wss, _) <- runLayout (W.Workspace "" l (Just $ W.Stack { W.focus=1, W.up=[],W.down=[1..nscr-1] })) rtrect
       windows $ \ws@W.StackSet{ W.current = v, W.visible = vs, W.hidden = hs } ->
           let x = W.workspace v
               (xs, ys) = splitAt (nscr - 1) $ map W.workspace vs ++ hs
               (notEmpty -> s :| ss) = map snd wss
           in  ws { W.current = W.Screen x 0 (SD s)
                  , W.visible = zipWith3 W.Screen xs [1 ..] $ map SD ss
                  , W.hidden  = ys }

-- | Modify current screen.
layoutSplitScreen :: LayoutClass l Int => Int -> l Int -> X ()
layoutSplitScreen nscr _ | nscr < 1 = trace $ "Can't layoutSplitScreen with only " ++ show nscr ++ " screens."
layoutSplitScreen nscr l =
    do rect <- gets $ screenRect . W.screenDetail . W.current . windowset
       (wss, _) <- runLayout (W.Workspace "" l (Just $ W.Stack { W.focus=1, W.up=[],W.down=[1..nscr-1] })) rect
       windows $ \ws@W.StackSet{ W.current = c, W.visible = vs, W.hidden = hs } ->
           let x = W.workspace c
               (xs, ys) = splitAt (nscr - 1) hs
               (notEmpty -> s :| ss) = map snd wss
           in  ws { W.current = W.Screen x (W.screen c) (SD s)
                  , W.visible = zipWith3 W.Screen xs [(W.screen c+1) ..] (map SD ss) ++
                                map (\v -> if W.screen v>W.screen c then v{W.screen = W.screen v + fromIntegral (nscr-1)} else v) vs
                  , W.hidden  = ys }

windowRectangle :: WindowAttributes -> Rectangle
windowRectangle a = Rectangle (fromIntegral $ wa_x a)     (fromIntegral $ wa_y a)
                              (fromIntegral $ wa_width a) (fromIntegral $ wa_height a)

newtype FixedLayout a = FixedLayout [Rectangle] deriving (Read,Show)

instance LayoutClass FixedLayout a where
    doLayout (FixedLayout rs) _ s = return (zip (W.integrate s) rs, Nothing)

fixedLayout :: [Rectangle] -> FixedLayout a
fixedLayout = FixedLayout
