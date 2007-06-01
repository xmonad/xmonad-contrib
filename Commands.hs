-----------------------------------------------------------------------------
-- |
-- Module      :  XMonadContrib.Commands
-- Copyright   :  (c) David Glasser 2007
-- 
-- Maintainer  :  glasser@mit.edu
-- Stability   :  stable
-- Portability :  portable
--
-----------------------------------------------------------------------------
--
-- Allows you to run internal xmonad commands (X () actions) using
-- a dmenu menu in addition to key bindings.  Requires dmenu and
-- the Dmenu XMonadContrib module.
--
-- To use, modify your Config.hs to:
--
--      import XMonadContrib.Commands
--
-- and add a keybinding to the runCommand action:
--
--     , ((modMask .|. controlMask, xK_y), runCommand)
--
-- and define the list commands:
--
--     commands = defaultCommands
--
-- Finally, add the following lines to Config.hs-boot:
-- 
--     import XMonad (X)
--     workspaces  :: Int
--     commands    :: [(String, X ())]
--
-- A popup menu of internal xmonad commands will appear.  You can
-- change the commands by changing the contents of the list
-- 'commands'.  (If you like it enough, you may even want to get rid
-- of many of your other key bindings!)

module XMonadContrib.Commands where

import XMonad
import Operations
import {-# SOURCE #-} Config (workspaces, commands)
import XMonadContrib.Dmenu (dmenu)

import qualified Data.Map as M
import System.Exit
import Data.Maybe

commandMap :: M.Map String (X ())
commandMap = M.fromList commands

workspaceCommands :: [(String, X ())]
workspaceCommands = [((m ++ show i), f (fromIntegral i))
                         | i <- [0 .. workspaces - 1]
                         , (f, m) <- [(view, "view"), (shift, "shift")]
                    ]

screenCommands :: [(String, X ())]
screenCommands = [((m ++ show sc), screenWorkspace (fromIntegral sc) >>= f)
                      | sc <- [0, 1]::[Int] -- TODO: adapt to screen changes
                      , (f, m) <- [(view, "screen"), (shift, "screen-to-")]
                 ]

defaultCommands :: [(String, X ())]
defaultCommands = workspaceCommands ++ screenCommands
                  ++ [ ("shrink", sendMessage Shrink)
                     , ("expand", sendMessage Expand)
                     , ("restart-wm", restart Nothing True)
                     , ("restart-wm-no-resume", restart Nothing False)
                     , ("layout", switchLayout)
                     , ("xterm", spawn "xterm")
                     , ("run", spawn "exe=`dmenu_path | dmenu -b` && exec $exe")
                     , ("kill", kill)
                     , ("refresh", refresh)
                     , ("focus-up", focusUp)
                     , ("focus-down", focusDown)
                     , ("swap-up", swapUp)
                     , ("swap-down", swapDown)
                     , ("swap-master", swapMaster)
                     , ("sink", withFocused sink)
                     , ("quit-wm", io $ exitWith ExitSuccess)
                     ]

runCommand :: X ()
runCommand = do
  choice <- dmenu (M.keys commandMap)
  fromMaybe (return ()) (M.lookup choice commandMap)
