-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Hooks.OnPropertyChange
-- Description :  Apply a manageHook on a property (e.g., @WM_CLASS@) change
-- Copyright   :  (c) Brandon S Allbery, 2015
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  allbery.b@gmail.com
-- Stability   :  unstable
-- Portability :  not portable
--
-- Module to apply a ManageHook to an already-mapped window when a property
-- changes. This would commonly be used to match browser windows by title,
-- since the final title will only be set after (a) the window is mapped,
-- (b) its document has been loaded, (c) all load-time scripts have run.
-- (Don't blame browsers for this; it's inherent in HTML and the DOM. And
-- changing title dynamically is explicitly permitted by ICCCM and EWMH;
-- you don't really want to have your editor window umapped/remapped to
-- show the current document and modified state in the titlebar, do you?)
--
-- This is a handleEventHook that triggers on a PropertyChange event. It
-- currently ignores properties being removed, in part because you can't
-- do anything useful in a ManageHook involving nonexistence of a property.
--
-- This module could also be useful for Electron applications like Spotify
-- which sets its WM_CLASS too late for window manager to map it properly.
--
-----------------------------------------------------------------------------

module XMonad.Hooks.OnPropertyChange (
  -- * Usage
  -- $usage
  onXPropertyChange,
  onTitleChange,
  onClassChange,
) where

import XMonad
import XMonad.Prelude

-- $usage
-- You can use this module with the following in your @xmonad.hs@:
--
-- > import XMonad.Hooks.OnPropertyChange
--
-- Enable it by including in you handleEventHook definition:
--
-- >  main = xmonad $ def
-- >      { ...
-- >      , handleEventHook = onXPropertyChange "WM_NAME" (title =? "Spotify" --> doShift "5")
-- >      , ...
-- >      }
--
-- Or you could create a dynamicManageHook as below:
--
-- > myDynamicManageHook :: ManageHook
-- > myDynamicManageHook =
-- >  composeAll
-- >    [ className =? "Spotify" --> doShift (myWorkspaces !! 4),
-- >      title =? "maybe_special_terminal" <||> title =? "special_terminal" --> doCenterFloat,
-- >      className =? "dynamicApp" <&&> title =? "dynamic_app" --> doCenterFloat
-- >    ]
--
-- And then use it in your handleEventHookDefinition:
--
-- >  main = xmonad $ def
-- >      { ...
-- >      , handleEventHook = onXPropertyChange "WM_NAME" myDynamicManageHook
-- >      , ...
-- >      }
--

-- |
-- Run a 'ManageHook' when a specific property is changed on a window. Note
-- that this will run on any window which changes the property, so you should
-- be very specific in your 'ManageHook' matching (lots of windows change
-- their titles on the fly!):
--
-- > onXPropertyChange "WM_NAME" (className =? "Iceweasel" <&&> title =? "whatever" --> doShift "2")
--
-- Note that the fixity of (-->) won't allow it to be mixed with ($), so you
-- can't use the obvious $ shorthand.
--
-- > onXPropertyChange "WM_NAME" $ title =? "Foo" --> doFloat -- won't work!
--
-- Consider instead phrasing it like any
-- other 'ManageHook':
--
-- >  main = xmonad $ def
-- >      { ...
-- >      , handleEventHook = onXPropertyChange "WM_NAME" myDynHook
-- >      , ...
-- >      }
-- >
-- >    myDynHook = composeAll [...]
--
onXPropertyChange :: String -> ManageHook -> Event -> X All
onXPropertyChange prop hook PropertyEvent { ev_window = w, ev_atom = a, ev_propstate = ps } = do
  pa <- getAtom prop
  when (ps == propertyNewValue && a == pa) $ do
    g <- appEndo <$> userCodeDef (Endo id) (runQuery hook w)
    windows g
  return mempty -- so anything else also processes it
onXPropertyChange _ _ _ = return mempty

-- | A shorthand for dynamic titles; i.e., applications changing their
-- @WM_NAME@ property.
onTitleChange :: ManageHook -> Event -> X All
-- strictly, this should also check _NET_WM_NAME. practically, both will
-- change and each gets its own PropertyEvent, so we'd need to record that
-- we saw the event for that window and ignore the second one. Instead, just
-- trust that nobody sets only _NET_WM_NAME. (I'm sure this will prove false,
-- since there's always someone who can't bother being compliant.)
onTitleChange = onXPropertyChange "WM_NAME"

-- | A shorthand for dynamic resource and class names; i.e.,
-- applications changing their @WM_CLASS@ property.
onClassChange :: ManageHook -> Event -> X All
onClassChange = onXPropertyChange "WM_CLASS"
