-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Actions.Plane
-- Copyright   :  (c) Marco Túlio Gontijo e Silva <marcot@riseup.net>,
--                    Leonardo Serra <leoserra@minaslivre.org>
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  Marco Túlio Gontijo e Silva <marcot@riseup.net>
-- Stability   :  unstable
-- Portability :  unportable
--
-- This module has functions to navigate through workspaces in a bidimensional
-- manner.  It allows the organization of workspaces in lines, and provides
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
    , Lines (..)

    -- * Navigating through workspaces
    , planeShift
    , planeMove
    )
    where

import Control.Monad
import Data.List hiding (union)
import Data.Maybe

import XMonad
import XMonad.StackSet hiding (workspaces)
import XMonad.Util.Run

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
-- >     [ ((keyMask .|. m, keySym), function (Lines 3) Finite direction)
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

-- | The number of lines in which the workspaces will be arranged.  It's
-- possible to use a number of lines that is not a divisor of the number of
-- workspaces, but the results are better when using a divisor.  If it's not a
-- divisor, the last line will have the remaining workspaces.
data Lines
    = GConf     -- ^ Use @gconftool-2@ to find out the number of lines.
    | Lines Int -- ^ Specify the number of lines explicity.

-- $navigating
--
-- There're two parameters that must be provided to navigate, and it's a good
-- idea to use them with the same values in each keybinding.
--
-- The first is the number of lines in which the workspaces are going to be
-- organized.  It's possible to use a number of lines that is not a divisor
-- of the number of workspaces, but the results are better when using a
-- divisor.  If it's not a divisor, the last line will have the remaining
-- workspaces.
--
-- The other one is 'Limits'.

-- | Shift a window to the next workspace in 'Direction'.  Note that this will
-- also move to the next workspace.
planeShift
    :: Lines
    -> Limits
    -> Direction
    -> X ()
planeShift = plane shift'

shift' ::
    (Eq s, Eq i, Ord a) => i -> StackSet i l a s sd -> StackSet i l a s sd
shift' area = greedyView area . shift area

-- | Move to the next workspace in 'Direction'.
planeMove :: Lines -> Limits -> Direction -> X ()
planeMove = plane greedyView

plane ::
    (WorkspaceId -> WindowSet -> WindowSet) -> Lines -> Limits -> Direction ->
    X ()
plane function numberLines_ limits direction = do
    state <- get
    xconf <- ask

    numberLines <-
        liftIO $
        case numberLines_ of
            Lines numberLines__ ->
                return numberLines__
            GConf               ->
                do
                    numberLines__ <-
                        runProcessWithInput gconftool parameters ""
                    case reads numberLines__ of
                        [(numberRead, _)] -> return numberRead
                        _                 ->
                            do
                                trace $
                                    "XMonad.Actions.Plane: Could not parse the output of " ++ gconftool ++
                                    unwords parameters ++ ": " ++ numberLines__ ++ "; assuming 1."
                                return 1

    let
        circular_ :: Int
        circular_ = circular currentWS

        circular :: Int -> Int
        circular =
            [ onLine   pred
            , onColumn pred
            , onLine   succ
            , onColumn succ
            ]
            !! fromEnum direction

        onLine :: (Int -> Int) -> Int -> Int
        onLine f currentWS_
            | line < areasLine = mod_ columns
            | otherwise        = mod_ areasColumn
            where
                line, column :: Int
                (line, column) = split currentWS_

                mod_ :: Int -> Int
                mod_ columns_ = compose line $ mod (f column) columns_

        onColumn :: (Int -> Int) -> Int -> Int
        onColumn f currentWS_
            | column < areasColumn || areasColumn == 0  = mod_ numberLines
            | otherwise                                 = mod_ $ pred numberLines
            where
                line, column :: Int
                (line, column) = split currentWS_

                mod_ :: Int -> Int
                mod_ lines_ = compose (mod (f line) lines_) column

        compose :: Int -> Int -> Int
        compose line column = line * columns + column

        split :: Int -> (Int, Int)
        split currentWS_ =
            (operation div, operation mod)
            where
                operation :: (Int -> Int -> Int) -> Int
                operation f = f currentWS_ columns

        areasLine :: Int
        areasLine = div areas columns

        areasColumn :: Int
        areasColumn = mod areas columns

        columns :: Int
        columns =
            if mod areas numberLines == 0 then preColumns else preColumns + 1

        currentWS :: Int
        currentWS = fromJust mCurrentWS

        preColumns :: Int
        preColumns = div areas numberLines

        mCurrentWS :: Maybe Int
        mCurrentWS = elemIndex (currentTag $ windowset state) areaNames

        areas :: Int
        areas = length areaNames

        run :: (Int -> Int) -> X ()
        run f = windows $ function $ areaNames !! f currentWS

        areaNames :: [String]
        areaNames = workspaces $ config xconf

    when (isJust mCurrentWS) $
        case limits of
        Finite   ->
            when ((replicate 2 (circular_ < currentWS) ++ replicate 2 (circular_ > currentWS)) !! fromEnum direction)
            $ run circular

        Circular ->
            run circular

gconftool :: String
gconftool = "gconftool-2"

parameters :: [String]
parameters = ["--get", "/apps/panel/applets/workspace_switcher_screen0/prefs/num_rows"]