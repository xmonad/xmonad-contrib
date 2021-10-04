-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Hooks.ToggleHook
-- Description :  Hook and keybindings for toggling hook behavior.
-- Copyright   :  Ben Boeckel <mathstuf@gmail.com>
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Ben Boeckel <mathstuf@gmail.com>
-- Stability   :  unstable
-- Portability :  unportable
--
-- Hook and keybindings for toggling hook behavior.
-----------------------------------------------------------------------------

module XMonad.Hooks.ToggleHook ( -- * Usage
                                 -- $usage

                                 -- * The hook
                                 toggleHook
                               , toggleHook'

                                 -- * Actions
                               , hookNext
                               , toggleHookNext
                               , hookAllNew
                               , toggleHookAllNew

                                 -- * Queries
                               , willHook
                               , willHookNext
                               , willHookAllNew

                                 -- * Status bar utilities
                                 -- $pp
                               , willHookNextPP
                               , willHookAllNewPP
                               , runLogHook ) where

import Prelude hiding (all)

import XMonad
import XMonad.Prelude (guard, join)
import qualified XMonad.Util.ExtensibleState as XS

import Control.Arrow (first, second)

import Data.Map

{- Helper functions -}

_set :: String -> ((a -> a) -> (Bool, Bool) -> (Bool, Bool)) -> a -> X ()
_set n f b = modify' n (f $ const b)

_toggle :: String -> ((Bool -> Bool) -> (Bool, Bool) -> (Bool, Bool)) -> X ()
_toggle n f = modify' n (f not)

_get :: String -> ((Bool, Bool) -> a) -> X a
_get n f = XS.gets $ f . (findWithDefault (False, False) n . hooks)

_pp :: String -> ((Bool, Bool) -> Bool) -> String -> (String -> String) -> X (Maybe String)
_pp n f s st = (\b -> guard b >> Just (st s)) <$> _get n f

{- The current state is kept here -}

newtype HookState = HookState { hooks :: Map String (Bool, Bool) } deriving (Read, Show)

instance ExtensionClass HookState where
    initialValue = HookState empty
    extensionType = PersistentExtension

modify' :: String -> ((Bool, Bool) -> (Bool, Bool)) -> X ()
modify' n f = XS.modify (HookState . setter . hooks)
    where
        setter m = insert n (f (findWithDefault (False, False) n m)) m

-- $usage
-- This module provides actions (that can be set as keybindings)
-- to be able to cause hooks to be occur on a conditional basis.
--
-- You can use it by including the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Hooks.ToggleHook
--
-- and adding 'toggleHook name hook' to your 'ManageHook' where @name@ is the
-- name of the hook and @hook@ is the hook to execute based on the state.
--
-- > myManageHook = toggleHook "float" doFloat <+> manageHook def
--
-- Additionally, toggleHook' is provided to toggle between two hooks (rather
-- than on/off).
--
-- > myManageHook = toggleHook' "oldfocus" (const id) W.focusWindow <+> manageHook def
--
-- The 'hookNext' and 'toggleHookNext' functions can be used in key
-- bindings to set whether the hook is applied or not.
--
-- > , ((modm, xK_e), toggleHookNext "float")
--
-- 'hookAllNew' and 'toggleHookAllNew' are similar but float all
-- spawned windows until disabled again.
--
-- > , ((modm, xK_r), toggleHookAllNew "float")

-- | This 'ManageHook' will selectively apply a hook as set
-- by 'hookNext' and 'hookAllNew'.
toggleHook :: String -> ManageHook -> ManageHook
toggleHook n h = toggleHook' n h idHook

toggleHook' :: String -> ManageHook -> ManageHook -> ManageHook
toggleHook' n th fh = do m <- liftX $ XS.gets hooks
                         (next, all) <- return $ findWithDefault (False, False) n m
                         liftX $ XS.put $ HookState $ insert n (False, all) m
                         if next || all then th else fh

-- | @hookNext name True@ arranges for the next spawned window to
-- have the hook @name@ applied, @hookNext name False@ cancels it.
hookNext :: String -> Bool -> X ()
hookNext n = _set n first

toggleHookNext :: String -> X ()
toggleHookNext n = _toggle n first

-- | @hookAllNew name True@ arranges for new windows to
-- have the hook @name@ applied, @hookAllNew name False@ cancels it
hookAllNew :: String -> Bool -> X ()
hookAllNew n = _set n second

toggleHookAllNew :: String -> X ()
toggleHookAllNew n = _toggle n second

-- | Query what will happen at the next ManageHook call for the hook @name@.
willHook :: String -> X Bool
willHook n = willHookNext n <||> willHookAllNew n

-- | Whether the next window will trigger the hook @name@.
willHookNext :: String -> X Bool
willHookNext n = _get n fst

-- | Whether new windows will trigger the hook @name@.
willHookAllNew :: String -> X Bool
willHookAllNew n = _get n snd

-- $pp
-- The following functions are used to display the current
-- state of 'hookNext' and 'hookAllNew' in your
-- "XMonad.Hooks.StatusBar". 'willHookNextPP' and
-- 'willHookAllNewPP' should be added to the
-- 'XMonad.Hooks.StatusBar.PP.ppExtras' field of your
-- "XMonad.Hooks.StatusBar.PP".
--
-- Use 'runLogHook' to refresh the output of your 'logHook', so
-- that the effects of a 'hookNext'/... will be visible
-- immediately:
--
-- > , ((modm, xK_e), toggleHookNext "float" >> runLogHook)
--
-- The @String -> String@ parameters to 'willHookNextPP' and
-- 'willHookAllNewPP' will be applied to their output, you
-- can use them to set the text color, etc., or you can just
-- pass them 'id'.

willHookNextPP :: String -> (String -> String) -> X (Maybe String)
willHookNextPP n = _pp n fst "Next"

willHookAllNewPP :: String -> (String -> String) -> X (Maybe String)
willHookAllNewPP n = _pp n snd "All"

runLogHook :: X ()
runLogHook = join $ asks $ logHook . config
