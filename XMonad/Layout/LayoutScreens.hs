{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.LayoutScreens
-- Copyright   :  (c) David Roundy <droundy@darcs.net>
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  David Roundy <droundy@darcs.net>
-- Stability   :  unstable
-- Portability :  unportable
--
-----------------------------------------------------------------------------

module XMonad.Layout.LayoutScreens (
                                    -- * Usage
                                    -- $usage
                                    layoutScreens, fixedLayout
                                   ) where

import XMonad
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
-- >   , ((modMask x .|. shiftMask,                 xK_space), layoutScreens 2 (TwoPane 0.5 0.5))
-- >   , ((modMask x .|. controlMask .|. shiftMask, xK_space), rescreen)
--
-- Another example use would be to handle a scenario where xrandr didn't
-- work properly (e.g. a VNC X server in my case) and you want to be able
-- to resize your screen (e.g. to match the size of a remote VNC client):
--
-- > import XMonad.Layout.LayoutScreens
--
-- >   , ((modMask x .|. shiftMask, xK_space),
-- >        layoutScreens 1 (fixedLayout [Rectangle 0 0 1024 768]))
-- >   , ((modMask x .|. controlMask .|. shiftMask, xK_space), rescreen)
--
-- For detailed instructions on editing your key bindings, see
-- "XMonad.Doc.Extending#Editing_key_bindings".

layoutScreens :: LayoutClass l Int => Int -> l Int -> X ()
layoutScreens nscr _ | nscr < 1 = trace $ "Can't layoutScreens with only " ++ show nscr ++ " screens."
layoutScreens nscr l =
    do rtrect <- asks theRoot >>= getWindowRectangle
       (wss, _) <- runLayout (W.Workspace "" l (Just $ W.Stack { W.focus=1, W.up=[],W.down=[1..nscr-1] })) rtrect
       windows $ \ws@(W.StackSet { W.current = v, W.visible = vs, W.hidden = hs }) ->
           let (x:xs, ys) = splitAt nscr $ map W.workspace (v:vs) ++ hs
               s:ss = map snd wss
           in  ws { W.current = W.Screen x 0 (SD s)
                  , W.visible = zipWith3 W.Screen xs [1 ..] $ map SD ss
                  , W.hidden  = ys }

getWindowRectangle :: Window -> X Rectangle
getWindowRectangle w = withDisplay $ \d ->
    do a <- io $ getWindowAttributes d w
       return $ Rectangle (fromIntegral $ wa_x a)     (fromIntegral $ wa_y a)
                          (fromIntegral $ wa_width a) (fromIntegral $ wa_height a)

data FixedLayout a = FixedLayout [Rectangle] deriving (Read,Show)

instance LayoutClass FixedLayout a where
    doLayout (FixedLayout rs) _ s = return (zip (W.integrate s) rs, Nothing)

fixedLayout :: [Rectangle] -> FixedLayout a
fixedLayout = FixedLayout
