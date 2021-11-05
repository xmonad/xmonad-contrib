-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Hooks.TaffybarPagerHints
-- Description :  Export additional X properties for [taffybar](https://github.com/taffybar/taffybar).
-- Copyright   :  (c) 2020 Ivan Malison
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  Ivan Malison <ivanmalison@gmail.com>
-- Stability   :  unstable
-- Portability :  unportable
--
-- This module exports additional X properties that allow
-- [taffybar](https://github.com/taffybar/taffybar) to understand the state of
-- XMonad.
--
-----------------------------------------------------------------------------

module XMonad.Hooks.TaffybarPagerHints (
    -- $usage
    pagerHints,
    pagerHintsLogHook,
    pagerHintsEventHook,

    setCurrentLayoutProp,
    setVisibleWorkspacesProp,
    ) where

import Codec.Binary.UTF8.String (encode)
import Foreign.C.Types (CInt)

import XMonad
import XMonad.Prelude
import qualified XMonad.StackSet as W

-- $usage
--
-- You can use this module with the following in your @xmonad.hs@ file:
--
-- > import XMonad.Hooks.TaffybarPagerHints (pagerHints)
-- >
-- > main = xmonad $ ewmh $ pagerHints $ def
-- > ...

-- | The \"Current Layout\" custom hint.
xLayoutProp :: X Atom
xLayoutProp = getAtom "_XMONAD_CURRENT_LAYOUT"

-- | The \"Visible Workspaces\" custom hint.
xVisibleProp :: X Atom
xVisibleProp = getAtom "_XMONAD_VISIBLE_WORKSPACES"

-- | Add support for the \"Current Layout\" and \"Visible Workspaces\" custom
-- hints to the given config.
pagerHints :: XConfig a -> XConfig a
pagerHints c =
  c { handleEventHook = handleEventHook c <> pagerHintsEventHook
    , logHook = logHook c <> pagerHintsLogHook
    }

-- | Update the current values of both custom hints.
pagerHintsLogHook :: X ()
pagerHintsLogHook = do
  withWindowSet
    (setCurrentLayoutProp . description . W.layout . W.workspace . W.current)
  withWindowSet
    (setVisibleWorkspacesProp . map (W.tag . W.workspace) . W.visible)

-- | Set the value of the \"Current Layout\" custom hint to the one given.
setCurrentLayoutProp :: String -> X ()
setCurrentLayoutProp l = withDisplay $ \dpy -> do
  r <- asks theRoot
  a <- xLayoutProp
  c <- getAtom "UTF8_STRING"
  let l' = map fromIntegral (encode l)
  io $ changeProperty8 dpy r a c propModeReplace l'

-- | Set the value of the \"Visible Workspaces\" hint to the one given.
setVisibleWorkspacesProp :: [String] -> X ()
setVisibleWorkspacesProp vis = withDisplay $ \dpy -> do
  r  <- asks theRoot
  a  <- xVisibleProp
  c  <- getAtom "UTF8_STRING"
  let vis' = map fromIntegral $ concatMap ((++[0]) . encode) vis
  io $ changeProperty8 dpy r a c propModeReplace vis'

-- | Handle all \"Current Layout\" events received from pager widgets, and
-- set the current layout accordingly.
pagerHintsEventHook :: Event -> X All
pagerHintsEventHook ClientMessageEvent
                      { ev_message_type = mt
                      , ev_data = d
                      } = withWindowSet $ \_ -> do
  a <- xLayoutProp
  when (mt == a) $ sendLayoutMessage d
  return (All True)
pagerHintsEventHook _ = return (All True)

-- | Request a change in the current layout by sending an internal message
-- to XMonad.
sendLayoutMessage :: [CInt] -> X ()
sendLayoutMessage (x:_) | x < 0     = sendMessage FirstLayout
                        | otherwise = sendMessage NextLayout
sendLayoutMessage [] = return ()
