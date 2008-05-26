-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Actions.Plane
-- Copyright   :  (c) Malebria <malebria@riseup.net>,
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  Malebria <malebria@riseup.net>
-- Stability   :  unstable
-- Portability :  unportable
--
-- This module has functions to navigate through workspaces in a bidimensional
-- manner.  It allows the organization of workspaces in columns, and provides
-- functions to move and shift windows in all four directions (left, up, right
-- and down) possible in a surface.
--
-- This functionality was inspired by GNOME (finite) and KDE (infinite)
-- keybindings for workspace navigation, and by "XMonad.Actions.CycleWS" for
-- the idea of applying this approach to XMonad.
-----------------------------------------------------------------------------

module XMonad.Actions.Plane
    (
    -- * Usage
    -- $usage

    -- * Data types
    Direction (..)
    , Limits (..)

    -- * Navigating through workspaces
    -- $navigating
    , planeShift
    , planeMove
    )
    where

import Control.Monad
import Data.List hiding (union)
import Data.Maybe

import XMonad
import XMonad.StackSet hiding (workspaces)

-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@ file:
--
-- > import XMonad.Actions.Plane
-- >
-- > main = xmonad defaultConfig {keys = myKeys}
-- >
-- > myKeys conf = union (keys defaultConfig conf) $ myNewKeys conf
-- >
-- > myNewkeys (XConfig {modMask = m}) =
-- >     fromList
-- >     [ ((keyMask .|. m, keySym), function 3 Finite direction)
-- >     | (keySym, direction) <- zip [xK_Left .. xK_Down] $ enumFrom ToLeft
-- >     , (keyMask, function) <- [(0, planeMove), (shiftMask, planeShift)]
-- >     ]
--
-- For detailed instructions on editing your key bindings, see
-- "XMonad.Doc.Extending#Editing_key_bindings".

-- | Direction to go in the plane.
data Direction =  ToLeft | ToUp | ToRight | ToDown deriving Enum

-- | Defines whether it's a finite or a circular organization of workspaces.
data Limits
    = Finite   -- ^ When you're at a edge of the plane, there's no way to move
               -- to the next region.
    | Circular -- ^ If you try to move, you'll get to the other edge, on the
               -- other side.
    deriving Eq

-- $navigating
--
-- There're two parameters that must be provided to navigate, and it's a good
-- idea to use them with the same values in each keybinding.
--
-- The first is the number of columns in which the workspaces are going to be
-- organized.  It's possible to use a number of columns that is not a divisor
-- of the number of workspaces, but the results are better when using a
-- divisor.  If it's not a divisor, the last line will have the remaining
-- workspaces.
--
-- The other one is 'Limits'.

-- | Shift a window to the next workspace in 'Direction'.  Note that this will
-- also move to the next workspace.
planeShift
    :: Int  -- ^ Number of columns.
    -> Limits
    -> Direction
    -> X ()
planeShift = plane shift'

shift' ::
    (Eq s, Eq i, Ord a) => i -> StackSet i l a s sd -> StackSet i l a s sd
shift' area = greedyView area . shift area

-- | Move to the next workspace in 'Direction'.
planeMove
    :: Int  -- ^ Number of columns.
    -> Limits
    -> Direction
    -> X ()
planeMove = plane greedyView

plane ::
    (WorkspaceId -> WindowSet -> WindowSet) -> Int -> Limits -> Direction ->
    X ()
plane function columns limits direction = do
    state <- get
    xconf <- ask
    let vertical f =
            if column >= areasColumn
                then mod (f currentWS columns) $ areasLine * columns
                else mod (f currentWS columns) $ (areasLine + 1) * columns

        horizontal f =
            if line < areasLine
                then mod (f column) columns + lineNumber
                else mod (f column) areasColumn + lineNumber

        areasLine = div areas columns
        areasColumn = mod areas columns
        lineNumber = line * columns
        line = div currentWS columns
        column = mod currentWS columns
        currentWS = fromJust mCurrentWS
        mCurrentWS = elemIndex (currentTag $ windowset state) areaNames
        run condition position =
            when (limits == Circular || condition) $
            windows $ function $ areaNames !! position
        areas = length areaNames
        areaNames = workspaces $ config $ xconf

    when (isJust mCurrentWS) $
        case direction of
            ToUp    -> run (line   /= 0                ) $ vertical (-)
            ToDown  -> run (currentWS + columns < areas) $ vertical (+)
            ToLeft  -> run (column /= 0                ) $ horizontal pred
            ToRight -> run (column /= columns - 1      ) $ horizontal succ
