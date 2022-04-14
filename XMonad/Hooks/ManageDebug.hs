-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Hooks.ManageDebug
-- Description :  A manageHook and associated logHook for debugging "ManageHooks".
-- Copyright   :  (c) Brandon S Allbery KF8NH, 2014
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  allbery.b@gmail.com
-- Stability   :  unstable
-- Portability :  not portable
--
-- A @manageHook@ and associated @logHook@ for debugging 'ManageHook's.
-- Simplest usage: wrap your xmonad config in the @debugManageHook@ combinator.
-- Or use @debugManageHookOn@ for a triggerable version, specifying the
-- triggering key sequence in 'XMonad.Util.EZConfig' syntax. Or use the
-- individual hooks in whatever way you see fit.
--
-----------------------------------------------------------------------------
--
--

module XMonad.Hooks.ManageDebug (debugManageHook
                                ,debugManageHookOn
                                ,manageDebug
                                ,maybeManageDebug
                                ,manageDebugLogHook
                                ,debugNextManagedWindow
                                ) where

import           XMonad
import           XMonad.Hooks.DebugStack
import           XMonad.Util.DebugWindow
import           XMonad.Util.EZConfig
import qualified XMonad.Util.ExtensibleState                                                 as XS

-- state for manageHook debugging to trigger logHook debugging
data MSDFinal = DoLogHook | SkipLogHook deriving Show
data MSDTrigger = MSDActivated | MSDInactive deriving Show
data ManageStackDebug = MSD MSDFinal MSDTrigger deriving Show
instance ExtensionClass ManageStackDebug where
  initialValue = MSD SkipLogHook MSDInactive

-- | A combinator to add full 'ManageHook' debugging in a single operation.
debugManageHook :: XConfig l -> XConfig l
debugManageHook cf = cf {logHook    = manageDebugLogHook <+> logHook    cf
                        ,manageHook = manageDebug        <+> manageHook cf
                        }

-- | A combinator to add triggerable 'ManageHook' debugging in a single operation.
--   Specify a key sequence as a string in 'XMonad.Util.EZConfig' syntax; press
--   this key before opening the window to get just that logged.
debugManageHookOn :: String -> XConfig l -> XConfig l
debugManageHookOn key cf = cf {logHook    = manageDebugLogHook <+> logHook    cf
                              ,manageHook = maybeManageDebug   <+> manageHook cf
                              }
                           `additionalKeysP`
                           [(key,debugNextManagedWindow)]

-- | Place this at the start of a 'ManageHook', or possibly other places for a
--   more limited view. It will show the current 'StackSet' state and the new
--   window, and set a flag so that @manageDebugLogHook@ will display the
--   final 'StackSet' state.
--
--   Note that the initial state shows only the current workspace; the final
--   one shows all workspaces, since your 'manageHook' might use e.g. 'doShift',
manageDebug :: ManageHook
manageDebug = do
  w <- ask
  liftX $ do
    trace "== manageHook; current stack =="
    debugStackString >>= trace
    ws <- debugWindow w
    trace $ "new window:\n  " ++ ws
    -- technically we don't care about go here, since only maybeManageDebug
    -- uses it
    XS.modify $ \(MSD _ go') -> MSD DoLogHook go'
  idHook

-- | @manageDebug@ only if the user requested it with @debugNextManagedWindow@.
maybeManageDebug :: ManageHook
maybeManageDebug = do
  go <- liftX $ do
    MSD _ go' <- XS.get
    -- leave it active, as we may manage multiple windows before the logHook
    -- so we now deactivate it in the logHook
    return go'
  case go of
    MSDActivated -> manageDebug
    _            -> idHook

-- | If @manageDebug@ has set the debug-stack flag, show the stack.
manageDebugLogHook :: X ()
manageDebugLogHook = do
                       MSD log' _ <- XS.get
                       case log' of
                         DoLogHook -> do
                                  trace "== manageHook; final stack =="
                                  debugStackFullString >>= trace
                                  -- see comment in maybeManageDebug
                                  XS.put $ MSD SkipLogHook MSDInactive
                         _         -> idHook

-- | Request that the next window to be managed be @manageDebug@-ed. This can
--   be used anywhere an X action can, such as key bindings, mouse bindings
--   (presumably with 'const'), 'startupHook', etc.
debugNextManagedWindow :: X ()
debugNextManagedWindow = XS.modify $ \(MSD log' _) -> MSD log' MSDActivated
