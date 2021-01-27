-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Hooks.DynamicProperty
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
-----------------------------------------------------------------------------

module XMonad.Hooks.DynamicProperty where

import XMonad
import Data.Monoid
import Control.Monad (when)

-- |
-- Run a 'ManageHook' when a specific property is changed on a window. Note
-- that this will run on any window which changes the property, so you should
-- be very specific in your 'MansgeHook' matching (lots of windows change
-- their titles on the fly!):
--
-- dynamicPropertyChange "WM_NAME" (className =? "Iceweasel" <&&> title =? "whatever" --> doShift "2")
-- 
-- Note that the fixity of (-->) won't allow it to be mixed with ($), so you
-- can't use the obvious $ shorthand.
--
-- > dynamicPropertyChange "WM_NAME" $ title =? "Foo" --> doFloat -- won't work!
--
-- Consider instead phrasing it like any
-- other 'ManageHook':
--
-- >      , handleEventHook = dynamicPropertyChange "WM_NAME" myDynHook <+> handleEventHook baseConfig
-- > 
-- >    {- ... -}
-- > 
-- >    myDynHook = composeAll [...]
--
dynamicPropertyChange :: String -> ManageHook -> Event -> X All
dynamicPropertyChange prop hook PropertyEvent { ev_window = w, ev_atom = a, ev_propstate = ps } = do
  pa <- getAtom prop
  when (ps == propertyNewValue && a == pa) $ do
    g <- appEndo <$> userCodeDef (Endo id) (runQuery hook w)
    windows g
  return mempty -- so anything else also processes it
dynamicPropertyChange _ _ _ = return mempty

-- | A shorthand for the most common case, dynamic titles
dynamicTitle :: ManageHook -> Event -> X All
-- strictly, this should also check _NET_WM_NAME. practically, both will
-- change and each gets its own PropertyEvent, so we'd need to record that
-- we saw the event for that window and ignore the second one. Instead, just
-- trust that nobody sets only _NET_WM_NAME. (I'm sure this will prove false,
-- since there's always someone who can't bother being compliant.)
dynamicTitle = dynamicPropertyChange "WM_NAME"
