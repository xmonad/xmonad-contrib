-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Hooks.Qubes
-- Description :  Use Qubes OS label colours for window borders.
-- Copyright   :  (c) Roland C. Dowdeswell <elric@imrryr.org>
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  Roland C. Dowdeswell <elric@imrryr.org>
-- Stability   :  unstable
-- Portability :  unportable
--
-- Refresh window borders from the @_QUBES_LABEL_COLOR@ property set by
-- Qubes OS so focused and unfocused windows match their qube label.
--
-----------------------------------------------------------------------------
module XMonad.Hooks.Qubes
    ( -- * Usage
      -- $usage
      qubesLogHook
    ) where

import Data.Bits
import Foreign.C.Types (CLong)

import           XMonad
import qualified XMonad.StackSet as W

-- $usage
-- You can use this module with the following in your @xmonad.hs@:
--
-- > import XMonad
-- > import XMonad.Hooks.Qubes
-- >
-- > main = xmonad def
-- >     { logHook = qubesLogHook
-- >     }

-- | Refresh visible window borders using their Qubes label colours.
qubesLogHook :: X ()
qubesLogHook = withWindowSet $ \s -> withDisplay $ \d -> do
    let visibleWins = (W.integrate' . W.stack . W.workspace . W.current $ s)
                   ++ concatMap (W.integrate' . W.stack . W.workspace)
                                (W.visible s)
        focused = W.peek s
    mapM_ (setQubesBorder d focused) visibleWins

setQubesBorder :: Display -> Maybe Window -> Window -> X ()
setQubesBorder d focused w = do
    s <- getCardinalProperty d w "_QUBES_LABEL_COLOR"
    let border = qubesBorderColour s focused w
    io $ setWindowBorder d w border

qubesBorderColour :: Maybe [CLong] -> Maybe Window -> Window -> Pixel
qubesBorderColour s focused w =
    getColour s (if Just w == focused then focusIn else focusOut)

fadeByte :: Int -> Pixel -> Pixel
fadeByte sw c = (flip shiftL) sw $ (.&.) 0x7f $ shiftR c (sw + 1)

fadeColour :: EventType -> Pixel -> Pixel
fadeColour f c
    | f == focusIn = c
    | otherwise = (fadeByte 16 c) .|. (fadeByte 8 c) .|. fadeByte 0 c

getColour :: Maybe [CLong] -> EventType -> Pixel
getColour (Just [s]) f = fadeColour f (fromIntegral s)
getColour _ _ = 0x000000

getCardinalProperty :: Display -> Window -> String -> X (Maybe [CLong])
getCardinalProperty d w p = do
    a <- getAtom p
    io $ getWindowProperty32 d a w
