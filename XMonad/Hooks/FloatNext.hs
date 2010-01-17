{-# LANGUAGE DeriveDataTypeable #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Hooks.FloatNext
-- Copyright   :  Quentin Moser <moserq@gmail.com>
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Quentin Moser <moserq@gmail.com>
-- Stability   :  unstable
-- Portability :  unportable
--
-- Hook and keybindings for automatically sending the next
-- spawned window(s) to the floating layer.
--
-----------------------------------------------------------------------------

module XMonad.Hooks.FloatNext ( -- * Usage
                                -- $usage

                                -- * The hook
                                floatNextHook

                                -- * Actions
                              , floatNext
                              , toggleFloatNext
                              , floatAllNew
                              , toggleFloatAllNew

                                -- * Queries
                              , willFloatNext
                              , willFloatAllNew

                                -- * 'DynamicLog' utilities
                                -- $pp
                              , willFloatNextPP
                              , willFloatAllNewPP
                              , runLogHook ) where

import Prelude hiding (all)

import XMonad
import qualified XMonad.Util.ExtensibleState as XS

import Control.Monad (join,guard)
import Control.Applicative ((<$>))
import Control.Arrow (first, second)

{- Helper functions -}

_set :: ((a -> a) -> (Bool, Bool) -> (Bool, Bool)) -> a -> X ()
_set f b = modify' (f $ const b)

_toggle :: ((Bool -> Bool) -> (Bool, Bool) -> (Bool, Bool)) -> X ()
_toggle f = modify' (f not)

_get :: ((Bool, Bool) -> a) -> X a
_get f = XS.gets (f . getFloatMode)

_pp :: ((Bool, Bool) -> Bool) -> String -> (String -> String) -> X (Maybe String)
_pp f s st = (\b -> guard b >> Just (st s)) <$> _get f

{- The current state is kept here -}

data FloatMode = FloatMode { getFloatMode :: (Bool,Bool) } deriving (Typeable)

instance ExtensionClass FloatMode where
    initialValue = FloatMode (False,False)

modify' :: ((Bool,Bool) -> (Bool,Bool)) -> X ()
modify' f = XS.modify (FloatMode . f . getFloatMode)

-- $usage
-- This module provides actions (that can be set as keybindings)
-- to automatically send the next spawned window(s) to the floating
-- layer.
--
-- You can use it by including the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Hooks.FloatNext
--
-- and adding 'floatNextHook' to your 'ManageHook':
--
-- > myManageHook = floatNextHook <+> manageHook defaultConfig
--
-- The 'floatNext' and 'toggleFloatNext' functions can be used in key
-- bindings to float the next spawned window:
--
-- > , ((modm, xK_e), toggleFloatNext)
--
-- 'floatAllNew' and 'toggleFloatAllNew' are similar but float all
-- spawned windows until disabled again.
--
-- > , ((modm, xK_r), toggleFloatAllNew)

-- | This 'ManageHook' will selectively float windows as set
-- by 'floatNext' and 'floatAllNew'.
floatNextHook :: ManageHook
floatNextHook = do (next, all) <- liftX $ XS.gets getFloatMode
                   liftX $ XS.put $ FloatMode (False, all)
                   if next || all then doFloat else idHook

-- | @floatNext True@ arranges for the next spawned window to be
-- sent to the floating layer, @floatNext False@ cancels it.
floatNext :: Bool -> X ()
floatNext = _set first

toggleFloatNext :: X ()
toggleFloatNext = _toggle first

-- | @floatAllNew True@ arranges for new windows to be
-- sent to the floating layer, @floatAllNew False@ cancels it
floatAllNew :: Bool -> X ()
floatAllNew = _set second

toggleFloatAllNew :: X ()
toggleFloatAllNew = _toggle second

-- | Whether the next window will be set floating
willFloatNext :: X Bool
willFloatNext = _get fst

-- | Whether new windows will be set floating
willFloatAllNew :: X Bool
willFloatAllNew = _get snd

-- $pp
-- The following functions are used to display the current
-- state of 'floatNext' and 'floatAllNew' in your
-- 'XMonad.Hooks.DynamicLog.dynamicLogWithPP'.
-- 'willFloatNextPP' and 'willFloatAllNewPP' should be added
-- to the 'XMonad.Hooks.DynamicLog.ppExtras' field of your
-- 'XMonad.Hooks.DynamicLog.PP'.
--
-- Use 'runLogHook' to refresh the output of your 'logHook', so
-- that the effects of a 'floatNext'/... will be visible
-- immediately:
--
-- > , ((modm, xK_e), toggleFloatNext >> runLogHook)
--
-- The @String -> String@ parameters to 'willFloatNextPP' and
-- 'willFloatAllNewPP' will be applied to their output, you
-- can use them to set the text color, etc., or you can just
-- pass them 'id'.

willFloatNextPP :: (String -> String) -> X (Maybe String)
willFloatNextPP = _pp fst "Next"

willFloatAllNewPP :: (String -> String) -> X (Maybe String)
willFloatAllNewPP = _pp snd "All"

runLogHook :: X ()
runLogHook = join $ asks $ logHook . config
