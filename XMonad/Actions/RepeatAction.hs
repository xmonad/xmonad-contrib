{-# LANGUAGE LambdaCase #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Actions.RepeatAction
-- Description :  Repeat the last performed action.
-- Copyright   :  (c) 2022 Martin Kozlovsky
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  <kozlovsky.m7@gmail.com>
-- Stability   :  unstable
-- Portability :  not portable
--
-- Ability to repeat the last action.
--
-----------------------------------------------------------------------------

module XMonad.Actions.RepeatAction (
  -- * Usage
  -- $usage
  rememberAction,
  rememberActions,
  repeatLast,
) where

import XMonad

import qualified XMonad.Util.ExtensibleState as XS

-- $usage
--
-- You can use this module with the following in your @xmonad.hs@:
--
-- > import XMonad.Actions.RepeatAction
--
-- Then join a dedicated key to run the last action with the rest of your
-- key bindings using the 'rememberActions':
--
-- > rememberActions (modm, xK_period) [((modm, xK_c), kill), …]
--
-- It can be also used in the same way for "XMonad.Util.EZConfig":
--
-- > rememberActions "M-." [("M-c", kill), …]
--
-- For example, if you use 'XMonad.Util.EZConfig.additionalKeysP',
--
-- > main = xmonad $ … $ def
-- >   {
-- >     …
-- >   }
-- >  `additionalKeysP` myKeys
--
-- you would adjust the call to 'XMonad.Util.EZConfig.additionalKeysP'
-- like so:
--
-- > `additionalKeysP` (rememberActions "M-." myKeys)
--
-- For more detailed instructions on editing your key bindings, see
-- <https://xmonad.org/TUTORIAL.html the tutorial>.

newtype LastAction = LastAction { runLastAction :: X () }

instance ExtensionClass LastAction where
  initialValue = LastAction $ pure ()

-- | Transforms an action into an action that can be remembered and repeated.
rememberAction :: X () -> X ()
rememberAction x = userCode x >>= \case
  Nothing -> pure ()
  Just () -> XS.put (LastAction x)  -- Only remember action if nothing went wrong.

-- | Maps 'rememberAction' over a list of key bindings.
rememberActions' :: [(a, X ())] -> [(a, X ())]
rememberActions' = map (fmap rememberAction)

infixl 4 `rememberActions`
-- | Maps 'rememberAction' over a list of key bindings and adds a dedicated
-- key to repeat the last action.
rememberActions :: a -> [(a, X ())] -> [(a, X ())]
rememberActions key keyList = (key, repeatLast) : rememberActions' keyList

-- | Runs the last remembered action.
-- / Be careful not to include this action in the remembered actions! /
repeatLast :: X ()
repeatLast = XS.get >>= runLastAction
