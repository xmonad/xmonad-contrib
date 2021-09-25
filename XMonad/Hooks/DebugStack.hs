-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Hooks.DebugStack
-- Description :  Dump the state of the StackSet.
-- Copyright   :  (c) Brandon S Allbery KF8NH, 2014
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  allbery.b@gmail.com
-- Stability   :  unstable
-- Portability :  not portable
--
-- Dump the state of the 'StackSet'. A @logHook@ and @handleEventHook@ are
-- also provided.
--
-----------------------------------------------------------------------------

module XMonad.Hooks.DebugStack (debugStack
                               ,debugStackFull
                               ,debugStackString
                               ,debugStackFullString
                               ,debugStackLogHook
                               ,debugStackFullLogHook
                               ,debugStackEventHook
                               ,debugStackFullEventHook
                               ) where

import           XMonad.Core
import           XMonad.Prelude
import qualified XMonad.StackSet                                       as W

import           XMonad.Util.DebugWindow

import           Graphics.X11.Types                  (Window)
import           Graphics.X11.Xlib.Extras            (Event)

import           Data.Map                            (member)

-- | Print the state of the current window stack for the current workspace to
--   @stderr@, which for most installations goes to @~/.xsession-errors@.
--   "XMonad.Util.DebugWindow" is used to display the individual windows.
debugStack :: X ()
debugStack =  debugStackString >>= trace

-- | Print the state of the current window stack for all workspaces to
--   @stderr@, which for most installations goes to @~/.xsession-errors@.
--   "XMonad.Util.DebugWindow" is used to display the individual windows.
debugStackFull :: X ()
debugStackFull =  debugStackFullString >>= trace

-- | 'debugStack' packaged as a 'logHook'. (Currently this is identical.)
debugStackLogHook :: X ()
debugStackLogHook =  debugStack

-- | 'debugStackFull packaged as a 'logHook'. (Currently this is identical.)
debugStackFullLogHook :: X ()
debugStackFullLogHook =  debugStackFull

-- | 'debugStack' packaged as a 'handleEventHook'. You almost certainly do not
--   want to use this unconditionally, as it will cause massive amounts of
--   output and possibly slow @xmonad@ down severely.

debugStackEventHook   :: Event -> X All
debugStackEventHook _ =  debugStack >> return (All True)

-- | 'debugStackFull' packaged as a 'handleEventHook'. You almost certainly do
--   not want to use this unconditionally, as it will cause massive amounts of
--   output and possibly slow @xmonad@ down severely.

debugStackFullEventHook   :: Event -> X All
debugStackFullEventHook _ =  debugStackFull >> return (All True)

-- | Dump the state of the current workspace in the 'StackSet' as a multiline 'String'.
debugStackString :: X String
debugStackString =  withWindowSet $ debugStackWs . W.workspace . W.current

-- | Dump the state of all workspaces in the 'StackSet' as a multiline 'String'.
-- @@@ this is in stackset order, which is roughly lru-ish
debugStackFullString :: X String
debugStackFullString =  withWindowSet $ fmap (intercalate "\n") . mapM debugStackWs . W.workspaces

-- | Dump the state of a workspace in the current 'StackSet' as a multiline 'String'.
--   @
--   Workspace "foo::
--     mm
--   * ww
--    ^ww
--   @
--   * indicates the focused window, ^ indicates a floating window
debugStackWs   :: W.Workspace String (Layout Window) Window -> X String
debugStackWs w =  withWindowSet $ \ws -> do
  let cur = if wt == W.currentTag ws then " (current)" else ""
      wt  = W.tag w
  s <- emit ws $ W.integrate' . W.stack $ w
  return $ intercalate "\n" $ ("Workspace " ++ show wt ++ cur):s
  where
    emit       :: WindowSet -> [Window] -> X [String]
    emit _  [] =  return ["    -empty workspace-"]
    emit ww ws = do
      (_,ss) <- foldM emit' (ww,[]) ws
      return ss

    emit' :: (WindowSet,[String])
          -> Window
          -> X (WindowSet,[String])
    emit' (ws,a) w' = do
      let focus = if Just w' == W.peek ws then '*' else ' '
          float = if w' `member` W.floating ws then '^' else ' '
      s <- debugWindow w'
      return (ws,(focus:float:s):a)
