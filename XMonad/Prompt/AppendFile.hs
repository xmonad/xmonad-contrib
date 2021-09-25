-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Prompt.AppendFile
-- Description :  A prompt for appending a single line of text to a file.
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

                                 appendFilePrompt,
                                 appendFilePrompt',
                                 AppendFile,
                                ) where

import XMonad.Core
import XMonad.Prompt

import System.IO

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
-- >  , ((modm .|. controlMask, xK_n), appendFilePrompt def "/home/me/NOTES")
--
-- Additional notes can be added via regular Haskell or XMonad functions; for
-- example, to preface notes with the time they were made, one could write a
-- binding like
--
-- > ,  ((modm .|. controlMask, xK_n), do
-- >            spawn ("date>>"++"/home/me/NOTES")
-- >            appendFilePrompt def "/home/me/NOTES"
-- >        )
--
-- (Put the spawn on the line after the prompt to append the time instead.)
--
-- 'appendFilePrompt'' can be used to transform the string input in the prompt
-- before saving into the file. Previous example with date can be rewritten as:
--
-- > ,  ((modm .|. controlMask, xK_n), do
-- >            date <- io $ fmap (formatTime defaultTimeLocale "[%Y-%m-%d %H:%M] ") getZonedTime
-- >            appendFilePrompt' def (date ++) $ "/home/me/NOTES"
-- >        )
--
-- A benefit is that if the prompt is cancelled the date is not output to
-- the file too.
--
-- For detailed instructions on editing your key bindings, see
-- "XMonad.Doc.Extending#Editing_key_bindings".

newtype AppendFile = AppendFile FilePath

instance XPrompt AppendFile where
    showXPrompt (AppendFile fn) = "Add to " ++ fn ++ ": "

-- | Given an XPrompt configuration and a file path, prompt the user
--   for a line of text, and append it to the given file.
appendFilePrompt :: XPConfig -> FilePath -> X ()
appendFilePrompt c = appendFilePrompt' c id

-- | Given an XPrompt configuration, string transformation function
--   and a file path, prompt the user for a line of text, transform it
--   and append the result to the given file.
appendFilePrompt' :: XPConfig -> (String -> String) -> FilePath -> X ()
appendFilePrompt' c trans fn = mkXPrompt (AppendFile fn)
                                  c
                                  (const (return []))
                                  (doAppend trans fn)

-- | Append a string to a file.
doAppend :: (String -> String) -> FilePath -> String -> X ()
doAppend trans fn = io . withFile fn AppendMode . flip hPutStrLn . trans
