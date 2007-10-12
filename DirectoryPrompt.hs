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
import XMonadContrib.Run ( runProcessWithInput )

-- $usage
-- For an example usage see "XMonadContrib.WorkspaceDir"

data Dir = Dir String

instance XPrompt Dir where
    showXPrompt (Dir x) = x

directoryPrompt :: XPConfig -> String -> (String -> X ()) -> X ()
directoryPrompt c prom job = mkXPrompt (Dir prom) c getDirCompl job

getDirCompl :: String -> IO [String]
getDirCompl s = (filter notboring . lines) `fmap`
                runProcessWithInput "/bin/bash" [] ("compgen -A directory " ++ s ++ "\n")

notboring :: String -> Bool
notboring ('.':'.':_) = True
notboring ('.':_) = False
notboring _ = True
