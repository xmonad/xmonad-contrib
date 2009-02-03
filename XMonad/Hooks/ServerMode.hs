-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Hooks.ServerMode
-- Copyright   :  (c) Andrea Rossato and David Roundy 2007
-- License     :  BSD-style (see xmonad/LICENSE)
--
-- Maintainer  :  andrea.rossato@unibz.it
-- Stability   :  unstable
-- Portability :  unportable
--
-- This is an 'EventHook' that will receive commands from an external
-- client.
--
-- This is the example of a client:
--
-- > import Graphics.X11.Xlib
-- > import Graphics.X11.Xlib.Extras
-- > import System.Environment
-- > import Data.Char
-- >
-- > usage :: String -> String
-- > usage n = "Usage: " ++ n ++ " command number\nSend a command number to a running instance of XMonad"
-- >
-- > main :: IO ()
-- > main = do
-- >   args <- getArgs
-- >   pn <- getProgName
-- >   let com = case args of
-- >               [] -> error $ usage pn
-- >               w -> (w !! 0)
-- >   sendCommand com
-- >
-- > sendCommand :: String -> IO ()
-- > sendCommand s = do
-- >   d   <- openDisplay ""
-- >   rw  <- rootWindow d $ defaultScreen d
-- >   a <- internAtom d "XMONAD_COMMAND" False
-- >   allocaXEvent $ \e -> do
-- >                   setEventType e clientMessage
-- >                   setClientMessageEvent e rw a 32 (fromIntegral (read s)) currentTime
-- >                   sendEvent d rw False structureNotifyMask e
-- >                   sync d False
--
-- compile with: @ghc --make sendCommand.hs@
--
-- run with
--
-- > sendCommand command number
--
-- For instance:
--
-- > sendCommand 0
--
--  will ask to xmonad to print the list of command numbers in
-- stderr (so you can read it in @~\/.xsession-errors@).
-----------------------------------------------------------------------------

module XMonad.Hooks.ServerMode
    ( -- * Usage
      -- $usage
      ServerMode (..)
    , serverModeEventHook
    ) where

import Control.Monad (when)
import Data.List
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
-- Then edit your @handleEventHook@ by adding the 'serverModeEventHook':
--
-- > main = xmonad defaultConfig { handleEventHook = serverModeEventHook }
--

data ServerMode = ServerMode deriving ( Show, Read )

serverModeEventHook :: Event -> X All
serverModeEventHook (ClientMessageEvent {ev_message_type = mt, ev_data = dt}) = do
        d <- asks display
        a <- io $ internAtom d "XMONAD_COMMAND" False
        when (mt == a && dt /= []) $ do
             cl <- defaultCommands
             let listOfCommands = map (uncurry (++)) . zip (map show ([1..] :: [Int])) . map ((++) " - " . fst)
             case lookup (fromIntegral (head dt) :: Int) (zip [1..] cl) of
                  Just (c,_) -> runCommand' c
                  Nothing    -> mapM_ (io . hPutStrLn stderr) . listOfCommands $ cl
        return (All True)
serverModeEventHook _ = return (All True)
