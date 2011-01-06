-----------------------------------------------------------------------------
-- |
-- Module       : XMonad.Hooks.ICCCMFocus
-- License      : BSD
--
-- Maintainer   : Tony Morris <haskell@tmorris.net>
--
-- Implemented in your @logHook@, Java swing applications will not misbehave
-- when it comes to taking and losing focus.
--
-- This has been done by taking the patch in <http://code.google.com/p/xmonad/issues/detail?id=177> and refactoring it so that it can be included in @~\/.xmonad\/xmonad.hs@.
--
-- @
--    conf' =
--      conf {
--        logHook = takeTopFocus
--      }
-- @
-----------------------------------------------------------------------------
module XMonad.Hooks.ICCCMFocus
(
  takeFocusX
, takeTopFocus
) where

import XMonad
import XMonad.Hooks.SetWMName
import qualified XMonad.StackSet as W
import Control.Monad

takeFocusX ::
  Window
  -> X ()
takeFocusX w =
  withWindowSet . const $ do
    dpy       <- asks display
    wmtakef   <- atom_WM_TAKE_FOCUS
    wmprot    <- atom_WM_PROTOCOLS
    protocols <- io $ getWMProtocols dpy w
    when (wmtakef `elem` protocols) $
      io . allocaXEvent $ \ev -> do
          setEventType ev clientMessage
          setClientMessageEvent ev w wmprot 32 wmtakef currentTime
          sendEvent dpy w False noEventMask ev

-- | The value to add to your log hook configuration.
takeTopFocus ::
  X ()
takeTopFocus =
  (withWindowSet $ maybe (setFocusX =<< asks theRoot) takeFocusX . W.peek) >> setWMName "LG3D"  

