-----------------------------------------------------------------------------
-- |
-- Module      :  XMonadContrib.RotView
-- Copyright   :  (c) David Roundy <droundy@darcs.net>
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  David Roundy <droundy@darcs.net>
-- Stability   :  unstable
-- Portability :  unportable
--
-- Provides bindings to cycle through non-empty workspaces.
--
-----------------------------------------------------------------------------

module XMonadContrib.LayoutScreens (
                                    -- * Usage
                                    -- $usage
                                    layoutScreens
                                   ) where

import Control.Monad.State ( modify )
import Control.Monad.Reader ( asks )

import XMonad
import qualified StackSet as W
import qualified Operations as O
import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras

-- $usage

-- This module allows you to pretend that you have more than one screen by
-- dividing a single screen into multiple screens that xmonad will treat as
-- separate screens.  This should definitely be useful for testing the
-- behavior of xmonad under Xinerama, and it's possible that it'd also be
-- handy for use as an actual user interface, if you've got a very large
-- sceen and long for greater flexibility (e.g. being able to see your
-- email window at all times, a crude mimic of sticky windows).

-- You can use this module with the following in your Config.hs file:
-- 
-- > import XMonadContrib.LayoutScreens
--
-- >   , ((modMask .|. shiftMask, xK_space), layoutScreens 2 (twoPane 0.5 0.5))
-- >   , ((controlMask .|. modMask .|. shiftMask, xK_space), do layoutScreens 1 xineScreenLayout
--                                                              rescreen)

layoutScreens :: Int -> Layout Int -> X ()
layoutScreens nscr _ | nscr < 1 = trace $ "Can't layoutScreens with only " ++ show nscr ++ " screens."
layoutScreens nscr l =
    do rtrect <- asks theRoot >>= getWindowRectangle
       wss <- doLayout l rtrect W.Stack { W.focus=1, W.up=[],W.down=[1..nscr-1] }
       modify $ \s -> s { xineScreens = map snd wss
                        , statusGaps  = take nscr $ (statusGaps s) ++ repeat (0,0,0,0) }

       O.windows $ \ws@(W.StackSet { W.current = v, W.visible = vs, W.hidden = hs }) ->
           let (x:xs, ys) = splitAt nscr $ map W.workspace (v:vs) ++ hs
           in  ws { W.current = W.Screen x 0
                  , W.visible = zipWith W.Screen xs [1 ..]
                  , W.hidden  = ys }

getWindowRectangle :: Window -> X Rectangle
getWindowRectangle w = withDisplay $ \d ->
    do a <- io $ getWindowAttributes d w
       return $ Rectangle (fromIntegral $ wa_x a)     (fromIntegral $ wa_y a)
                          (fromIntegral $ wa_width a) (fromIntegral $ wa_height a)
