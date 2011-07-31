-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Prompt.RunOrRaise
-- Copyright   :  (C) 2008 Justin Bogner
-- License     :  BSD3
--
-- Maintainer  :  mail@justinbogner.com
-- Stability   :  unstable
-- Portability :  unportable
--
-- A prompt for XMonad which will run a program, open a file,
-- or raise an already running program, depending on context.
--
-----------------------------------------------------------------------------

module XMonad.Prompt.RunOrRaise
    ( -- * Usage
      -- $usage
      runOrRaisePrompt
    ) where

import XMonad hiding (config)
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Actions.WindowGo (runOrRaise)
import XMonad.Util.Run (runProcessWithInput)

import Prelude hiding (catch)
import Control.Exception
import Control.Monad (liftM, liftM2)
import System.Directory (doesDirectoryExist, doesFileExist, executable, getPermissions)

econst :: Monad m => a -> IOException -> m a
econst = const . return

{- $usage
1. In your @~\/.xmonad\/xmonad.hs@:

> import XMonad.Prompt
> import XMonad.Prompt.RunOrRaise

2. In your keybindings add something like:

>   , ((modm .|. controlMask, xK_x), runOrRaisePrompt defaultXPConfig)

For detailed instruction on editing the key binding see
"XMonad.Doc.Extending#Editing_key_bindings". -}

data RunOrRaisePrompt = RRP
instance XPrompt RunOrRaisePrompt where
    showXPrompt RRP = "Run or Raise: "

runOrRaisePrompt :: XPConfig -> X ()
runOrRaisePrompt c = do cmds <- io getCommands
                        mkXPrompt RRP c (getShellCompl cmds) open
open :: String -> X ()
open path = io (isNormalFile path) >>= \b ->
            if b
            then spawn $ "xdg-open \"" ++ path ++ "\""
            else uncurry runOrRaise . getTarget $ path
    where
      isNormalFile f = exists f >>= \e -> if e then notExecutable f else return False
      exists f = fmap or $ sequence [doesFileExist f,doesDirectoryExist f]
      notExecutable = fmap (not . executable) . getPermissions
      getTarget x = (x,isApp x)

isApp :: String -> Query Bool
isApp "firefox"     = className =? "Firefox-bin"     <||> className =? "Firefox"
isApp "thunderbird" = className =? "Thunderbird-bin" <||> className =? "Thunderbird"
isApp x = liftM2 (==) pid $ pidof x

pidof :: String -> Query Int
pidof x = io $ (runProcessWithInput "pidof" [x] [] >>= readIO) `catch` econst 0

pid :: Query Int
pid = ask >>= (\w -> liftX $ withDisplay $ \d -> getPID d w)
    where getPID d w = getAtom "_NET_WM_PID" >>= \a -> io $
                       liftM getPID' (getWindowProperty32 d a w)
          getPID' (Just (x:_)) = fromIntegral x
          getPID' (Just [])     = -1
          getPID' (Nothing)     = -1
