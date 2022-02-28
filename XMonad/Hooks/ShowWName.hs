{-# LANGUAGE InstanceSigs #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Hooks.ShowWName
-- Description :  Like 'XMonad.Layout.ShowWName', but as a logHook
-- Copyright   :  (c) 2022  Solid <soliditsallgood@mailbox.org>
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  soliditsallgood@mailbox.org
--
-- Flash the names of workspaces name when switching to them.  This is a
-- reimplementation of 'XMonad.Layout.ShowWName' as a logHook.
-----------------------------------------------------------------------------

module XMonad.Hooks.ShowWName (
  -- * Usage
  -- $usage
  showWNameLogHook,
  SWNConfig(..),
  flashName,
) where

import qualified XMonad.StackSet             as W
import qualified XMonad.Util.ExtensibleState as XS

import XMonad
import XMonad.Layout.ShowWName (SWNConfig (..))
import XMonad.Prelude
import XMonad.Util.XUtils (WindowConfig (..), showSimpleWindow)

import Control.Concurrent (threadDelay)

{- $usage

You can use this module with the following in your
@~\/.xmonad\/xmonad.hs@:

> import XMonad.Hooks.ShowWName
>
> main :: IO ()
> main = xmonad $ def
>   { logHook = showWNameLogHook def
>   }

Whenever a workspace gains focus, the above logHook will flash its name.
You can customise the duration of the flash, as well as colours by
customising the 'SWNConfig' argument that 'showWNameLogHook' takes.

Alternatively, you can also bind 'flashName' to a key and manually
invoke it when you want to know which workspace you are on.
-}

-- | LogHook for flashing the name of a workspace upon entering it.
showWNameLogHook :: SWNConfig -> X ()
showWNameLogHook cfg = do
  LastShown s <- XS.get
  foc         <- withWindowSet (pure . W.currentTag)
  unless (s == foc) $ do
    flashName cfg
    XS.put (LastShown foc)

-- | Flash the name of the currently focused workspace.
flashName :: SWNConfig -> X ()
flashName cfg = do
  n <- withWindowSet (pure . W.currentTag)
  showSimpleWindow cfg' [n] >>= \w -> void . xfork $ do
    dpy <- openDisplay ""
    threadDelay (fromEnum $ swn_fade cfg * 1000000) -- 1_000_000 needs GHC 8.6.x and up
    void $ destroyWindow dpy w
    closeDisplay dpy
 where
  cfg' :: WindowConfig
  cfg' = def{ winFont = swn_font cfg, winBg = swn_bgcolor cfg, winFg = swn_color cfg }

-- | Last shown workspace.
newtype LastShown = LastShown WorkspaceId
  deriving (Show, Read)

instance ExtensionClass LastShown where
  initialValue :: LastShown
  initialValue  = LastShown ""

  extensionType :: LastShown -> StateExtension
  extensionType = PersistentExtension
