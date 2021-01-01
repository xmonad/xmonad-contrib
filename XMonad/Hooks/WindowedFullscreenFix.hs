
-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Hooks.WindowedFullscreenFix
-- Copyright   :  (c) 2020 Leon Kowarschick
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  Leon Kowarschick. <thereal.elkowar@gmail.com>
-- Stability   :  unstable
-- Portability :  unportable
--
-- Windowed fullscreen describes the behaviour in which XMonad,
-- by default, does not automatically put windows that request being fullscreened
-- into actual fullscreen, but keeps them constrained
-- to their noral window dimensions, still rendering them in fullscreen.
--
-- With chromium based applications like Chrome, Discord and others this
-- can cause issues, where the window does not correctly see the size of the window
-- when displaying the fullscreen content, thus cutting off the window content.
--
-- This module works around that issue by forcing the window to recalculate their
-- dimensions after initiating fullscreen, thus making chrome-based applications
-- behave properly when in windowed fullscreen.
-----------------------------------------------------------------------------
module XMonad.Hooks.WindowedFullscreenFix
  ( -- * Usage
    -- $usage
    windowedFullscreenFixEventHook
  ) where


import XMonad
import Data.Monoid (All(All))
import Control.Monad (when)


-- $usage
-- Use this module by importing
--
-- > import XMonad.Hooks.WindowedFullscreenFix
--
-- and then registering the provided eventHook in your handleEventHook:
--
-- > handleEventHook = handleEventHook def <+> windowedFullscreenFixEventHook
--

-- | Fixes fullscreen behaviour of chromium based apps by quickly applying and undoing a resize.
-- This causes chromium to recalculate the fullscreen window
-- dimensions to match the actual "windowed fullscreen" dimensions.
windowedFullscreenFixEventHook :: Event -> X All
windowedFullscreenFixEventHook (ClientMessageEvent _ _ _ dpy win typ (_:dats)) = do
  wmstate <- getAtom "_NET_WM_STATE"
  fullscreen <- getAtom "_NET_WM_STATE_FULLSCREEN"
  when (typ == wmstate && fromIntegral fullscreen `elem` dats) $ do
    withWindowAttributes dpy win $ \attrs ->
      liftIO $ resizeWindow dpy win (fromIntegral $ wa_width attrs - 1) (fromIntegral $ wa_height attrs)
    withWindowAttributes dpy win $ \attrs ->
      liftIO $ resizeWindow dpy win (fromIntegral $ wa_width attrs + 1) (fromIntegral $ wa_height attrs)
  return $ All True
windowedFullscreenFixEventHook _ = return $ All True