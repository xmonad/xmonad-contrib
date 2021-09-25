-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Actions.Plane
-- Description :  Navigate through workspaces in a bidimensional manner.
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

    -- * Key bindings
    , planeKeys

    -- * Navigating through workspaces
    , planeShift
    , planeMove
    )
    where

import Data.Map (Map, fromList)

import XMonad.Prelude
import XMonad
import XMonad.StackSet hiding (workspaces)
import XMonad.Util.Run

-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@ file:
--
-- > import XMonad.Actions.Plane
-- > import Data.Map (union)
-- >
-- > main = xmonad def {keys = myKeys}
-- >
-- > myKeys conf = union (keys def conf) $ myNewKeys conf
-- >
-- > myNewKeys (XConfig {modMask = modm}) = planeKeys modm (Lines 3) Finite
--
-- For detailed instructions on editing your key bindings, see
-- "XMonad.Doc.Extending#Editing_key_bindings".

-- | Direction to go in the plane.
data Direction =  ToLeft | ToUp | ToRight | ToDown deriving Enum

-- | Defines the behaviour when you're trying to move out of the limits.
data Limits
    = Finite   -- ^ Ignore the function call, and keep in the same workspace.
    | Circular -- ^ Get on the other side, like in the Snake game.
    | Linear   -- ^ The plan comes as a row, so it goes to the next or prev if
               -- the workspaces were numbered.
    deriving Eq

-- | The number of lines in which the workspaces will be arranged.  It's
-- possible to use a number of lines that is not a divisor of the number of
-- workspaces, but the results are better when using a divisor.  If it's not a
-- divisor, the last line will have the remaining workspaces.
data Lines
    = GConf     -- ^ Use @gconftool-2@ to find out the number of lines.
    | Lines Int -- ^ Specify the number of lines explicitly.

-- | This is the way most people would like to use this module.  It attaches the
-- 'KeyMask' passed as a parameter with 'xK_Left', 'xK_Up', 'xK_Right' and
-- 'xK_Down', associating it with 'planeMove' to the corresponding 'Direction'.
-- It also associates these bindings with 'shiftMask' to 'planeShift'.
planeKeys :: KeyMask -> Lines -> Limits -> Map (KeyMask, KeySym) (X ())
planeKeys modm ln limits =
  fromList $
  [ ((keyMask, keySym), function ln limits direction)
  | (keySym, direction) <- zip [xK_Left .. xK_Down] $ enumFrom ToLeft
  , (keyMask, function) <- [(modm, planeMove), (shiftMask .|. modm, planeShift)]
  ]

-- | Shift a window to the next workspace in 'Direction'.  Note that this will
-- also move to the next workspace.  It's a good idea to use the same 'Lines'
-- and 'Limits' for all the bindings.
planeShift :: Lines -> Limits -> Direction -> X ()
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
    st <- get
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
        notBorder :: Bool
        notBorder = (replicate 2 (circular_ < currentWS) ++ replicate 2 (circular_ > currentWS)) !! fromEnum direction

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

        linear :: Int -> Int
        linear =
            [ onLine   pred . onColumn pred
            , onColumn pred . onLine pred
            , onLine   succ . onColumn succ
            , onColumn succ . onLine succ
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
        mCurrentWS = elemIndex (currentTag $ windowset st) areaNames

        areas :: Int
        areas = length areaNames

        run :: (Int -> Int) -> X ()
        run f = windows $ function $ areaNames !! f currentWS

        areaNames :: [String]
        areaNames = workspaces $ config xconf

    when (isJust mCurrentWS) $
        case limits of
        Finite   -> when notBorder $ run circular
        Circular -> run circular
        Linear -> if notBorder then run circular else run linear

gconftool :: String
gconftool = "gconftool-2"

parameters :: [String]
parameters = ["--get", "/apps/panel/applets/workspace_switcher_screen0/prefs/num_rows"]
