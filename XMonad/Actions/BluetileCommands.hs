----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Actions.BluetileCommands
-- Description :  Interface with the [Bluetile](https://hackage.haskell.org/package/bluetile) tiling window manager.
-- Copyright   :  (c) Jan Vornberger 2009
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  jan.vornberger@informatik.uni-oldenburg.de
-- Stability   :  unstable
-- Portability :  not portable
--
-- This is a list of selected commands that can be made available using
-- "XMonad.Hooks.ServerMode" to allow external programs to control
-- the window manager. Bluetile (<http://projects.haskell.org/bluetile/>)
-- uses this to enable its dock application to do things like changing
-- workspaces and layouts.
--
-----------------------------------------------------------------------------

module XMonad.Actions.BluetileCommands (
    -- * Usage
    -- $usage
    bluetileCommands
    ) where

import XMonad
import qualified XMonad.StackSet as W
import System.Exit

-- $usage
--
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- >    import XMonad.Hooks.ServerMode
-- >    import XMonad.Actions.BluetileCommands
--
-- Then edit your @handleEventHook@:
--
-- > main = xmonad def { handleEventHook = serverModeEventHook' bluetileCommands }
--
-- See the documentation of "XMonad.Hooks.ServerMode" for details on
-- how to actually invoke the commands from external programs.

workspaceCommands :: Int -> X [(String, X ())]
workspaceCommands sid = asks (workspaces . config) >>= \spaces -> return
                            [( "greedyView" ++ show i,
                                activateScreen sid >> windows (W.greedyView i))
                                | i <- spaces ]

layoutCommands :: Int -> [(String, X ())]
layoutCommands sid = [ ("layout floating"    , activateScreen sid >>
                                                    sendMessage (JumpToLayout "Floating"))
                     , ("layout tiled1"      , activateScreen sid >>
                                                    sendMessage (JumpToLayout "Tiled1"))
                     , ("layout tiled2"      , activateScreen sid >>
                                                    sendMessage (JumpToLayout "Tiled2"))
                     , ("layout fullscreen"  , activateScreen sid >>
                                                    sendMessage (JumpToLayout "Fullscreen"))
                     ]

masterAreaCommands :: Int -> [(String, X ())]
masterAreaCommands sid = [ ("increase master n", activateScreen sid >>
                                                    sendMessage (IncMasterN 1))
                         , ("decrease master n", activateScreen sid >>
                                                    sendMessage (IncMasterN (-1)))
                         ]

quitCommands :: [(String, X ())]
quitCommands = [ ("quit bluetile", io exitSuccess)
               , ("quit bluetile and start metacity", restart "metacity" False)
               ]

bluetileCommands :: X [(String, X ())]
bluetileCommands = do
    let restartCommand = [ ("restart bluetile", restart "bluetile" True) ]
    wscmds0 <- workspaceCommands 0
    wscmds1 <- workspaceCommands 1
    return $ restartCommand
                ++ wscmds0 ++ layoutCommands 0 ++ masterAreaCommands 0 ++ quitCommands
                ++ wscmds1 ++ layoutCommands 1 ++ masterAreaCommands 1 ++ quitCommands

activateScreen :: Int -> X ()
activateScreen sid = screenWorkspace (S sid) >>= flip whenJust (windows . W.view)
