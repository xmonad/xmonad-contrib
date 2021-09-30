-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Prompt.Directory
-- Description :  A directory prompt for XMonad.
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
                             directoryMultipleModes',
                             Dir
                              ) where

import XMonad.Prelude ( sort )

import XMonad
import XMonad.Prompt
import XMonad.Prompt.Shell ( compgenDirectories )

-- $usage
-- For an example usage see "XMonad.Layout.WorkspaceDir"

data Dir = Dir String ComplCaseSensitivity (String -> X ())

instance XPrompt Dir where
    showXPrompt (Dir x _ _) = x
    completionFunction (Dir _ csn _) = getDirCompl csn
    modeAction (Dir _ _ f) buf auto =
      let dir = if null auto then buf else auto
      in f dir

directoryPrompt :: XPConfig -> String -> (String -> X ()) -> X ()
directoryPrompt c prom f = mkXPrompt (Dir prom csn f) c (getDirCompl csn) f
    where csn = complCaseSensitivity c

-- | A @XPType@ entry suitable for using with @mkXPromptWithModes@.
directoryMultipleModes :: String            -- ^ Prompt.
                       -> (String -> X ())  -- ^ Action.
                       -> XPType
directoryMultipleModes = directoryMultipleModes' CaseSensitive

-- | Like @directoryMultipleModes@ with a parameter for completion case-sensitivity.
directoryMultipleModes' :: ComplCaseSensitivity -- ^ Completion case sensitivity.
                        -> String               -- ^ Prompt.
                        -> (String -> X ())     -- ^ Action.
                        -> XPType
directoryMultipleModes' csn p f = XPT (Dir p csn f)

getDirCompl :: ComplCaseSensitivity -> String -> IO [String]
getDirCompl csn s = sort . filter notboring . lines <$> compgenDirectories csn s

notboring :: String -> Bool
notboring ('.':'.':_) = True
notboring ('.':_) = False
notboring _ = True
