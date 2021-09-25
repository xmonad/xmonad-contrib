-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Actions.Commands
-- Description :  Run internal xmonad commands using a dmenu menu.
-- Copyright   :  (c) David Glasser 2007
-- License     :  BSD3
--
-- Maintainer  :  glasser@mit.edu
-- Stability   :  stable
-- Portability :  portable
--
-- Allows you to run internal xmonad commands (X () actions) using
-- a dmenu menu in addition to key bindings.  Requires dmenu and
-- the Dmenu XMonad.Actions module.
--
-----------------------------------------------------------------------------

module XMonad.Actions.Commands (
                             -- * Usage
                             -- $usage
                             commandMap,
                             runCommand,
                             runCommandConfig,
                             runCommand',
                             workspaceCommands,
                             screenCommands,
                             defaultCommands
                              ) where

import XMonad
import XMonad.StackSet hiding (workspaces)
import XMonad.Util.Dmenu (dmenu)

import qualified Data.Map as M
import System.Exit
import XMonad.Prelude

-- $usage
--
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- >    import XMonad.Actions.Commands
--
-- Then add a keybinding to the runCommand action:
--
-- >    , ((modm .|. controlMask, xK_y), commands >>= runCommand)
--
-- and define the list of commands you want to use:
--
-- >    commands :: X [(String, X ())]
-- >    commands = defaultCommands
--
-- Whatever key you bound to will now cause a popup menu of internal
-- xmonad commands to appear.  You can change the commands by changing
-- the contents of the list returned by 'commands'.  (If you like it
-- enough, you may even want to get rid of many of your other key
-- bindings!)
--
-- For detailed instructions on editing your key bindings, see
-- "XMonad.Doc.Extending#Editing_key_bindings".

-- | Create a 'Data.Map.Map' from @String@s to xmonad actions from a
--   list of pairs.
commandMap :: [(String, X ())] -> M.Map String (X ())
commandMap = M.fromList

-- | Generate a list of commands to switch to\/send windows to workspaces.
workspaceCommands :: X [(String, X ())]
workspaceCommands = asks (workspaces . config) >>= \spaces -> return
                            [( m ++ show i, windows $ f i)
                                | i <- spaces
                                , (f, m) <- [(view, "view"), (shift, "shift")] ]

-- | Generate a list of commands dealing with multiple screens.
screenCommands :: [(String, X ())]
screenCommands = [( m ++ show sc, screenWorkspace (fromIntegral sc) >>= flip whenJust (windows . f))
                      | sc <- [0, 1]::[Int] -- TODO: adapt to screen changes
                      , (f, m) <- [(view, "screen"), (shift, "screen-to-")]
                 ]

-- | A nice pre-defined list of commands.
defaultCommands :: X [(String, X ())]
defaultCommands = do
    wscmds <- workspaceCommands
    return $ wscmds ++ screenCommands ++ otherCommands
 where
    otherCommands =
        [ ("shrink"              , sendMessage Shrink                               )
        , ("expand"              , sendMessage Expand                               )
        , ("next-layout"         , sendMessage NextLayout                           )
        , ("default-layout"      , asks (layoutHook . config) >>= setLayout         )
        , ("restart-wm"          , restart "xmonad" True                            )
        , ("restart-wm-no-resume", restart "xmonad" False                           )
        , ("xterm"               , spawn =<< asks (terminal .  config)              )
        , ("run"                 , spawn "exe=`dmenu_path | dmenu -b` && exec $exe" )
        , ("kill"                , kill                                             )
        , ("refresh"             , refresh                                          )
        , ("focus-up"            , windows focusUp                                  )
        , ("focus-down"          , windows focusDown                                )
        , ("swap-up"             , windows swapUp                                   )
        , ("swap-down"           , windows swapDown                                 )
        , ("swap-master"         , windows swapMaster                               )
        , ("sink"                , withFocused $ windows . sink                     )
        , ("quit-wm"             , io exitSuccess                                   )
        ]

-- | Given a list of command\/action pairs, prompt the user to choose a
--   command using dmenu and return the corresponding action.
runCommand :: [(String, X ())] -> X ()
runCommand = runCommandConfig dmenu


-- | Given a list of command\/action pairs, prompt the user to choose a
--   command using dmenu-compatible launcher and return the corresponding action.
--   See X.U.Dmenu for compatible launchers.
runCommandConfig :: ([String] -> X String) -> [(String, X ())] -> X()
runCommandConfig f cl = do
  let m = commandMap cl
  choice <- f (M.keys m)
  fromMaybe (return ()) (M.lookup choice m)

-- | Given the name of a command from 'defaultCommands', return the
--   corresponding action (or the null action if the command is not
--   found).
runCommand' :: String -> X ()
runCommand' c = do
  m <- fmap commandMap defaultCommands
  fromMaybe (return ()) (M.lookup c m)
