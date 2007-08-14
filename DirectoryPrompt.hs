-----------------------------------------------------------------------------
-- |
-- Module      :  XMonadContrib.DirectoryPrompt
-- Copyright   :  (C) 2007 Andrea Rossato, David Roundy
-- License     :  BSD3
-- 
-- Maintainer  :  droundy@darcs.net
-- Stability   :  unstable
-- Portability :  unportable
--
-- A directory prompt for XMonad
--
-----------------------------------------------------------------------------

module XMonadContrib.DirectoryPrompt (
                             -- * Usage
                             -- $usage
                             directoryPrompt
                              ) where

import XMonad
import XMonadContrib.XPrompt
import XMonadContrib.Dmenu ( runProcessWithInput )

-- $usage
--
-- 1. In xmonad.cabal change: 
--
-- > build-depends:      base>=2.0, X11>=1.2.1, X11-extras>=0.2, mtl>=1.0, unix>=1.0
--
-- to
--
-- > build-depends:      base>=2.0, X11>=1.2.1, X11-extras>=0.2, mtl>=1.0, unix>=1.0, readline >= 1.0
--
-- 2. In Config.hs add:
--
-- > import XMonadContrib.XPrompt
-- > import XMonadContrib.ShellPrompt
--
-- 3. In your keybindings add something like:
--
-- >   , ((modMask .|. controlMask, xK_x), shellPrompt defaultXPConfig)
--

data Dir = Dir String

instance XPrompt Dir where
    showXPrompt (Dir x) = x

directoryPrompt :: XPConfig -> String -> (String -> X ()) -> X ()
directoryPrompt c prom job = mkXPrompt (Dir prom) c getDirCompl job

getDirCompl :: String -> IO [String]
getDirCompl s = (filter notboring . lines) `fmap`
                runProcessWithInput "/bin/bash" [] ("compgen -A directory " ++ s ++ "\n")

notboring ('.':'.':_) = True
notboring ('.':_) = False
notboring _ = True
