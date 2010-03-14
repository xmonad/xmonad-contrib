-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Prompt.Directory
-- Copyright   :  (C) 2007 Andrea Rossato, David Roundy
-- License     :  BSD3
--
-- Maintainer  :
-- Stability   :  unstable
-- Portability :  unportable
--
-- A directory prompt for XMonad
--
-----------------------------------------------------------------------------

module XMonad.Prompt.Directory (
                             -- * Usage
                             -- $usage
                             directoryPrompt
                              ) where

import XMonad
import XMonad.Prompt
import XMonad.Util.Run ( runProcessWithInput )

-- $usage
-- For an example usage see "XMonad.Layout.WorkspaceDir"

data Dir = Dir String

instance XPrompt Dir where
    showXPrompt (Dir x) = x

directoryPrompt :: XPConfig -> String -> (String -> X ()) -> X ()
directoryPrompt c prom = mkXPrompt (Dir prom) c getDirCompl

getDirCompl :: String -> IO [String]
getDirCompl s = (filter notboring . lines) `fmap`
                runProcessWithInput "/bin/bash" [] ("compgen -A directory " ++ s ++ "\n")

notboring :: String -> Bool
notboring ('.':'.':_) = True
notboring ('.':_) = False
notboring _ = True
