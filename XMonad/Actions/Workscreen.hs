{-# LANGUAGE ViewPatterns #-}

-----------------------------------------------------------------------------
-- |
-- Module     :  XMonad.Actions.Workscreen
-- Description:  Display a set of workspaces on several screens.
-- Copyright  :  (c) 2012 kedals0
-- License    :  BSD3-style (see LICENSE)
--
-- Maintainer :  Dal <kedasl0@gmail.com>
-- Stability  :  unstable
-- Portability:  unportable
--
-- A workscreen permits to display a set of workspaces on several
-- screens. In xinerama mode, when a workscreen is viewed, workspaces
-- associated to all screens are visible.
--
-- The first workspace of a workscreen is displayed on first screen,
-- second on second screen, etc. Workspace position can be easily
-- changed. If the current workscreen is called again, workspaces are
-- shifted.
--
-- This also permits to see all workspaces of a workscreen even if just
-- one screen is present, and to move windows from workspace to workscreen.
-----------------------------------------------------------------------------

module XMonad.Actions.Workscreen (
  -- * Usage
  -- $usage
  configWorkscreen
  ,viewWorkscreen
  ,Workscreen(..)
  ,shiftToWorkscreen
  ,fromWorkspace
  ,expandWorkspace
  ,WorkscreenId
  ) where

import XMonad hiding (workspaces)
import XMonad.Prelude
import qualified XMonad.StackSet as W
import qualified XMonad.Util.ExtensibleState as XS
import XMonad.Actions.OnScreen

-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Actions.Workscreen
-- > myWorkspaces = let myOldWorkspaces = ["adm","work","mail"]
-- >                in Workscreen.expandWorkspace 2 myOldWorkspaces
-- > myStartupHook = do Workscreen.configWorkscreen (Workscreen.fromWorkspace 2 myWorkspaces)
-- >                    return ()
--
-- Then, replace normal workspace view and shift keybinding:
--
-- > [((m .|. modm, k), f i)
-- >      | (i, k) <- zip [0..] [1..12]
-- >      , (f, m) <- [(Workscreen.viewWorkscreen, 0), (Workscreen.shiftToWorkscreen, shiftMask)]]
--
-- For detailed instructions on editing your key bindings, see
-- "XMonad.Doc.Extending#Editing_key_bindings".


data Workscreen = Workscreen{workscreenId::Int,workspaces::[WorkspaceId]} deriving (Show)
type WorkscreenId=Int

data WorkscreenStorage = WorkscreenStorage WorkscreenId [Workscreen] deriving (Show)
instance ExtensionClass WorkscreenStorage where
  initialValue = WorkscreenStorage 0 []

-- | Helper to group workspaces. Multiply workspace by screens number.
expandWorkspace :: Int -> [WorkspaceId] -> [WorkspaceId]
expandWorkspace nscr = concatMap expandId
  where expandId wsId = let t = wsId ++ "_"
                        in map ((++) t . show ) [1..nscr]

-- | Create workscreen list from workspace list. Group workspaces to
-- packets of screens number size.
fromWorkspace :: Int -> [WorkspaceId] -> [Workscreen]
fromWorkspace n ws = zipWith Workscreen [0..] (fromWorkspace' n ws)
fromWorkspace' :: Int -> [WorkspaceId] -> [[WorkspaceId]]
fromWorkspace' _ [] = []
fromWorkspace' n ws = take n ws : fromWorkspace' n (drop n ws)

-- | Initial configuration of workscreens
configWorkscreen :: [Workscreen] -> X ()
configWorkscreen wscrn = XS.put (WorkscreenStorage 0 wscrn)

-- | View workscreen of index @WorkscreenId@. If current workscreen is asked
-- workscreen, workscreen's workspaces are shifted.
viewWorkscreen :: WorkscreenId -> X ()
viewWorkscreen wscrId = do (WorkscreenStorage c a) <- XS.get
                           let wscr = if wscrId == c
                                          then Workscreen wscrId $ shiftWs (workspaces $ a !! wscrId)
                                          else a !! wscrId
                               (x, notEmpty -> _ :| ys) = splitAt wscrId a
                               newWorkscreenStorage = WorkscreenStorage wscrId (x ++ [wscr] ++ ys)
                           windows (viewWorkscreen' wscr)
                           XS.put newWorkscreenStorage

viewWorkscreen' :: Workscreen -> WindowSet -> WindowSet
viewWorkscreen' (Workscreen _ ws) = \s -> foldl wsToSc' s (zip [0..] ws)
  where wsToSc' s (scr,wsId) = greedyViewOnScreen scr wsId s

shiftWs :: [WorkspaceId] -> [WorkspaceId]
shiftWs a = drop 1 a ++ take 1 a

-- | Shift a window on the first workspace of workscreen
-- @WorkscreenId@.
shiftToWorkscreen :: WorkscreenId -> X ()
shiftToWorkscreen wscrId = do (WorkscreenStorage _ a) <- XS.get
                              let ws = head . workspaces $ a !! wscrId
                              windows $ W.shift ws
