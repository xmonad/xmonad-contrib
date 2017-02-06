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
                             directoryPrompt,
                             directoryMultipleModes,
                             Dir
                              ) where

import XMonad
import XMonad.Prompt
import XMonad.Util.Run ( runProcessWithInput )

-- $usage
-- For an example usage see "XMonad.Layout.WorkspaceDir"

data Dir = Dir String (String -> X ())

instance XPrompt Dir where
    showXPrompt (Dir x _) = x
    completionFunction _ = getDirCompl
    modeAction (Dir _ f) buf auto =
      let dir = if null auto then buf else auto
      in f dir

directoryPrompt :: XPConfig -> String -> (String -> X ()) -> X ()
directoryPrompt c prom f = mkXPrompt (Dir prom f) c getDirCompl f

-- | A @XPType@ entry suitable for using with @mkXPromptWithModes@.
directoryMultipleModes :: String            -- ^ Prompt.
                       -> (String -> X ())  -- ^ Action.
                       -> XPType
directoryMultipleModes p f = XPT (Dir p f)

getDirCompl :: String -> IO [String]
getDirCompl s = (filter notboring . lines) `fmap`
                runProcessWithInput "bash" [] ("compgen -A directory " ++ s ++ "\n")

notboring :: String -> Bool
notboring ('.':'.':_) = True
notboring ('.':_) = False
notboring _ = True
