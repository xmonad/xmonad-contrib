-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Hooks.SetCursor
-- Copyright   :  (c) 2009 Nils Schweinsberg
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  Nils Schweinsberg <mail@n-sch.de>
-- Stability   :  unstable
-- Portability :  unportable
--
-- Set a default cursor on startup.
--
-- Thanks to Andres Salomon for his initial idea for this startup hook.
--
-----------------------------------------------------------------------------

module XMonad.Hooks.SetCursor (
    -- * Usage
    -- $usage
      setDefaultCursor
    ) where

import XMonad

{- $usage

To use this startup hook add a line to your startup hook:

> myStartupHook = do
>     setDefaultCursor 68
>     -- more stuff

Where @68@ is the default left pointer.

-}

-- | Set the default (root) cursor
setDefaultCursor :: Glyph -- ^ the cursor to use
    -> X ()
setDefaultCursor glyph = do
    dpy   <- asks display
    rootw <- asks theRoot
    liftIO $ do
        curs <- createFontCursor dpy glyph
        defineCursor dpy rootw curs
        flush dpy
        freeCursor dpy curs
