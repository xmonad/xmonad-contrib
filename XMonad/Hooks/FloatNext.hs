-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Hooks.FloatNext
-- Copyright   :  Quentin Moser <quentin.moser@unifr.ch>
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Quentin Moser <quentin.moser@unifr.ch>
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

import Control.Monad (join)
import Control.Applicative ((<$>))
import Control.Arrow (first, second)
import Control.Concurrent.MVar
import System.IO.Unsafe (unsafePerformIO)


{- Helper functions -}

modifyMVar2 :: MVar a -> (a -> a) -> IO ()
modifyMVar2 v f = modifyMVar_ v (return . f)

_set :: ((a -> a) -> (Bool, Bool) -> (Bool, Bool)) -> a -> X ()
_set f b = io $ modifyMVar2 floatModeMVar (f $ const b)

_toggle :: ((Bool -> Bool) -> (Bool, Bool) -> (Bool, Bool)) -> X ()
_toggle f = io $ modifyMVar2 floatModeMVar (f not)

_get :: ((Bool, Bool) -> a) -> X a
_get f = io $ f <$> readMVar floatModeMVar

_pp :: ((Bool, Bool) -> Bool) -> String -> (String -> String) -> X (Maybe String)
_pp f s st = _get f >>= \b -> if b then return $ Just $ st s else return Nothing


{- The current state is kept here -}

floatModeMVar :: MVar (Bool, Bool)
floatModeMVar = unsafePerformIO $ newMVar (False, False)


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
-- > , ((modMask, xK_e), toggleFloatNext)
--
-- 'floatAllNew' and 'toggleFloatAllNew' are similar but float all
-- spawned windows until disabled again.
--
-- > , ((modMask, xK_r), toggleFloatAllNew)


-- | This 'ManageHook' will selectively float windows as set
-- by 'floatNext' and 'floatAllNew'.
floatNextHook :: ManageHook
floatNextHook = do (next, all) <- io $ takeMVar floatModeMVar
                   io $ putMVar floatModeMVar (False, all)
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
-- > , ((modMask, xK_e), toggleFloatNext >> runLogHook)
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