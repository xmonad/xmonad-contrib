-----------------------------------------------------------------------------
-- |
-- Module      :  XMonadContrib.Commands
-- Copyright   :  (c) David Glasser 2007
-- License     :  BSD3
-- 
-- Maintainer  :  glasser@mit.edu
-- Stability   :  stable
-- Portability :  portable
--
-- Allows you to run internal xmonad commands (X () actions) using
-- a dmenu menu in addition to key bindings.  Requires dmenu and
-- the Dmenu XMonadContrib module.
--
-----------------------------------------------------------------------------

module XMonadContrib.Commands (
                             -- * Usage
                             -- $usage
                             commandMap,
                             runCommand,
                             runCommand',
                             workspaceCommands,
                             screenCommands,
                             defaultCommands
                              ) where

import XMonad
import Operations
import StackSet hiding (workspaces)
import XMonadContrib.Dmenu (dmenu)
import {-# SOURCE #-} Config (workspaces,serialisedLayouts, terminal)

import qualified Data.Map as M
import System.Exit
import Data.Maybe

-- $usage
--
-- To use, modify your Config.hs to:
--
-- >    import XMonadContrib.Commands
--
-- and add a keybinding to the runCommand action:
--
-- >    , ((modMask .|. controlMask, xK_y), runCommand commands)
--
-- and define the list commands:
--
-- >    commands :: [(String, X ())]
-- >    commands = defaultCommands
--
-- A popup menu of internal xmonad commands will appear.  You can
-- change the commands by changing the contents of the list
-- 'commands'.  (If you like it enough, you may even want to get rid
-- of many of your other key bindings!)

-- %def commands :: [(String, X ())]
-- %def commands = defaultCommands
-- %import XMonadContrib.Commands
-- %keybind , ((modMask .|. controlMask, xK_y), runCommand commands)

commandMap :: [(String, X ())] -> M.Map String (X ())
commandMap c = M.fromList c

workspaceCommands :: [(String, X ())]
workspaceCommands = [((m ++ show i), windows $ f i)
                         | i <- workspaces
                         , (f, m) <- [(view, "view"), (shift, "shift")]
                    ]

screenCommands :: [(String, X ())]
screenCommands = [((m ++ show sc), screenWorkspace (fromIntegral sc) >>= flip whenJust (windows . f))
                      | sc <- [0, 1]::[Int] -- TODO: adapt to screen changes
                      , (f, m) <- [(view, "screen"), (shift, "screen-to-")]
                 ]

defaultCommands :: [(String, X ())]
defaultCommands = workspaceCommands ++ screenCommands
                  ++ [ ("shrink"              , sendMessage Shrink                               )
                     , ("expand"              , sendMessage Expand                               )
                     , ("next-layout"         , sendMessage NextLayout                           )
                     , ("previous-layout"     , sendMessage PrevLayout                           )
                     , ("default-layout"      , setLayout (head serialisedLayouts)                 )
                     , ("restart-wm"          , sr >> restart Nothing True                       )
                     , ("restart-wm-no-resume", sr >> restart Nothing False                      )
                     , ("xterm"               , spawn terminal                                   )
                     , ("run"                 , spawn "exe=`dmenu_path | dmenu -b` && exec $exe" )
                     , ("kill"                , kill                                             )
                     , ("refresh"             , refresh                                          )
                     , ("focus-up"            , windows $ focusUp                                )
                     , ("focus-down"          , windows $ focusDown                              )
                     , ("swap-up"             , windows $ swapUp                                 )
                     , ("swap-down"           , windows $ swapDown                               )
                     , ("swap-master"         , windows $ swapMaster                             )
                     , ("sink"                , withFocused $ windows . sink                     )
                     , ("quit-wm"             , io $ exitWith ExitSuccess                        )
                     ]
    where sr = broadcastMessage ReleaseResources

runCommand :: [(String, X ())] -> X ()
runCommand cl = do
  let m = commandMap cl
  choice <- dmenu (M.keys m)
  fromMaybe (return ()) (M.lookup choice m)

runCommand' :: String -> X ()
runCommand' c = do
  let m = commandMap defaultCommands
  fromMaybe (return ()) (M.lookup c m)
