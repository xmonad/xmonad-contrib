-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Hooks.ServerMode
-- Copyright   :  (c) Peter Olson 2013 and Andrea Rossato and David Roundy 2007
-- License     :  BSD-style (see xmonad/LICENSE)
--
-- Maintainer  :  polson2@hawk.iit.edu
-- Stability   :  unstable
-- Portability :  unportable
--
-- This is an 'EventHook' that will receive commands from an external
-- client. Also consider "XMonad.Hooks.EwmhDesktops" together with
-- @wmctrl@.
--
-- See @scripts/xmonadctl.hs@ for the client.
--
-----------------------------------------------------------------------------

module XMonad.Hooks.ServerMode
    ( -- * Usage
      -- $usage
      serverModeEventHook
    , serverModeEventHook'
    , serverModeEventHookCmd
    , serverModeEventHookCmd'
    , serverModeEventHookF
    ) where

import Control.Monad (when)
import Data.Maybe
import Data.Monoid
import System.IO

import XMonad
import XMonad.Actions.Commands

-- $usage
-- You can use this module with the following in your
-- @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Hooks.ServerMode
--
-- Then edit your @handleEventHook@ by adding the appropriate event hook from below

-- | Executes a command of the list when receiving its index via a special ClientMessageEvent
-- (indexing starts at 1). Sending index 0 will ask xmonad to print the list of command numbers
-- in stderr (so that you can read it in @~\/.xsession-errors@). Uses "XMonad.Actions.Commands#defaultCommands" as the default.
--
-- > main = xmonad def { handleEventHook = serverModeEventHook }
-- 
-- > xmonadctl 0 # tells xmonad to output command list
-- > xmonadctl 1 # tells xmonad to switch to workspace 1
--
serverModeEventHook :: Event -> X All
serverModeEventHook = serverModeEventHook' defaultCommands

-- | serverModeEventHook' additionally takes an action to generate the list of
-- commands.
serverModeEventHook' :: X [(String,X ())] -> Event -> X All
serverModeEventHook' cmdAction ev = serverModeEventHookF "XMONAD_COMMAND" (sequence_ . map helper . words) ev
        where helper cmd = do cl <- cmdAction
                              case lookup cmd (zip (map show [1 :: Integer ..]) cl) of
                                Just (_,action) -> action
                                Nothing         -> mapM_ (io . hPutStrLn stderr) . listOfCommands $ cl
              listOfCommands cl = map (uncurry (++)) $ zip (map show ([1..] :: [Int])) $ map ((++) " - " . fst) cl


-- | Executes a command of the list when receiving its name via a special ClientMessageEvent.
-- Uses "XMonad.Actions.Commands#defaultCommands" as the default.
--
-- > main = xmonad def { handleEventHook = serverModeEventHookCmd }
--
-- > xmonadctl run # Tells xmonad to generate a run prompt
--
serverModeEventHookCmd :: Event -> X All
serverModeEventHookCmd = serverModeEventHookCmd' defaultCommands

-- | Additionally takes an action to generate the list of commands
serverModeEventHookCmd' :: X [(String,X ())] -> Event -> X All
serverModeEventHookCmd' cmdAction ev = serverModeEventHookF "XMONAD_COMMAND" (sequence_ . map helper . words) ev
        where helper cmd = do cl <- cmdAction
                              fromMaybe (io $ hPutStrLn stderr ("Couldn't find command " ++ cmd)) (lookup cmd cl)

-- | Listens for an atom, then executes a callback function whenever it hears it.
-- A trivial example that prints everything supplied to it on xmonad's standard out:
--
-- > main = xmonad def { handleEventHook = serverModeEventHookF "XMONAD_PRINT" (io . putStrLn) }
--
-- > xmonadctl -a XMONAD_PRINT "hello world"
--
serverModeEventHookF :: String -> (String -> X ()) -> Event -> X All
serverModeEventHookF key func (ClientMessageEvent {ev_message_type = mt, ev_data = dt}) = do
        d <- asks display
        atm <- io $ internAtom d key False
        when (mt == atm && dt /= []) $ do
         let atom = fromIntegral (head dt)
         cmd <- io $ getAtomName d atom
         case cmd of
              Just command -> func command
              Nothing -> io $ hPutStrLn stderr ("Couldn't retrieve atom " ++ (show atom))
        return (All True)
serverModeEventHookF _ _ _ = return (All True)
