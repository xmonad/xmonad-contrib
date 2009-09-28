-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Prompt.AppendFile
-- Copyright   :  (c) 2007 Brent Yorgey
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  <byorgey@gmail.com>
-- Stability   :  stable
-- Portability :  unportable
--
-- A prompt for appending a single line of text to a file.  Useful for
-- keeping a file of notes, things to remember for later, and so on---
-- using a keybinding, you can write things down just about as quickly
-- as you think of them, so it doesn't have to interrupt whatever else
-- you're doing.
--
-- Who knows, it might be useful for other purposes as well!
--
-----------------------------------------------------------------------------

module XMonad.Prompt.AppendFile (
                                 -- * Usage
                                 -- $usage

                                 appendFilePrompt
                                ) where

import XMonad.Core
import XMonad.Prompt

import System.IO
import Control.Exception

-- $usage
--
-- You can use this module by importing it, along with
-- "XMonad.Prompt", into your ~\/.xmonad\/xmonad.hs file:
--
-- > import XMonad.Prompt
-- > import XMonad.Prompt.AppendFile
--
-- and adding an appropriate keybinding, for example:
--
-- >  , ((modMask x .|. controlMask, xK_n), appendFilePrompt defaultXPConfig "/home/me/NOTES")
--
-- For detailed instructions on editing your key bindings, see
-- "XMonad.Doc.Extending#Editing_key_bindings".

data AppendFile = AppendFile FilePath

instance XPrompt AppendFile where
    showXPrompt (AppendFile fn) = "Add to " ++ fn ++ ": "

-- | Given an XPrompt configuration and a file path, prompt the user
--   for a line of text, and append it to the given file.
appendFilePrompt :: XPConfig -> FilePath -> X ()
appendFilePrompt c fn = mkXPrompt (AppendFile fn)
                                  c
                                  (const (return []))
                                  (doAppend fn)

-- | Append a string to a file.
doAppend :: FilePath -> String -> X ()
doAppend fn = io . bracket (openFile fn AppendMode) hClose . flip hPutStrLn
