-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Util.Replace
-- Description :  Implements a @--replace@ flag outside of core.
-- Copyright   :  (c) Jan Vornberger 2009
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  Adam Vogt <vogt.adam@gmail.com>
-- Stability   :  unstable
-- Portability :  unportable
--
-- Implements a @--replace@ behavior outside of core.
--
-----------------------------------------------------------------------------

-- refer to core patches:
-- http://article.gmane.org/gmane.comp.lang.haskell.xmonad/8358
module XMonad.Util.Replace
    ( -- * Usage
      -- $usage
      replace

      -- * Notes
      -- $shortcomings

      -- ** Implementing a @--replace@ flag
      -- $getArgs
    ) where

import XMonad
import XMonad.Prelude

-- $usage
-- You must run the 'replace' action before starting xmonad proper, this
-- results in xmonad replacing the currently running WM regardless of the
-- arguments it is run with:
--
-- > import XMonad
-- > import XMonad.Util.Replace
-- > main = do
-- >    replace
-- >    xmonad $ def { .... }
--

-- $shortcomings
-- This doesn't seem to work for replacing WMs that have been started
-- from within xmonad, such as with @'restart' "openbox" False@, but no other
-- WMs that implements --replace manage this either. 'replace' works for
-- replacing metacity when the full gnome-session is started at least.

-- $getArgs
-- You can use 'System.Environment.getArgs' to watch for an explicit
-- @--replace@ flag:
--
-- > import XMonad
-- > import XMonad.Util.Replace (replace)
-- > import Control.Monad (when)
-- > import System.Environment (getArgs)
-- >
-- > main = do
-- >    args <- getArgs
-- >    when ("--replace" `elem` args) replace
-- >    xmonad $ def { .... }
--
--
-- Note that your @~\/.xmonad/xmonad-$arch-$os@ binary is not run with the same
-- flags as the @xmonad@ binary that calls it. You may be able to work around
-- this by running your @~\/.xmonad/xmonad-$arch-$os@ binary directly, which is
-- otherwise not recommended.

-- | @replace@ must be run before xmonad starts to signals to compliant window
-- managers that they must exit and let xmonad take over.
replace :: IO ()
replace = do
    dpy   <- openDisplay ""
    let dflt = defaultScreen dpy

    rootw  <- rootWindow dpy dflt

    -- check for other WM
    wmSnAtom <- internAtom dpy ("WM_S" ++ show dflt) False
    currentWmSnOwner <- xGetSelectionOwner dpy wmSnAtom
    when (currentWmSnOwner /= 0) $ do
        putStrLn $ "Screen " ++ show dflt ++ " on display \""
                    ++ displayString dpy ++ "\" already has a window manager."

        -- prepare to receive destroyNotify for old WM
        selectInput dpy currentWmSnOwner structureNotifyMask

        -- create off-screen window
        netWmSnOwner <- allocaSetWindowAttributes $ \attributes -> do
            set_override_redirect attributes True
            set_event_mask attributes propertyChangeMask
            let screen = defaultScreenOfDisplay dpy
            let visual = defaultVisualOfScreen screen
            let attrmask = cWOverrideRedirect .|. cWEventMask
            createWindow dpy rootw (-100) (-100) 1 1 0 copyFromParent copyFromParent visual attrmask attributes

        -- try to acquire wmSnAtom, this should signal the old WM to terminate
        putStrLn "Replacing existing window manager..."
        xSetSelectionOwner dpy wmSnAtom netWmSnOwner currentTime

        -- SKIPPED: check if we acquired the selection
        -- SKIPPED: send client message indicating that we are now the WM

        -- wait for old WM to go away
        putStr "Waiting for other window manager to terminate... "
        fix $ \again -> do
            evt <- allocaXEvent $ \event -> do
                windowEvent dpy currentWmSnOwner structureNotifyMask event
                get_EventType event

            when (evt /= destroyNotify) again
        putStrLn "done"
    closeDisplay dpy
