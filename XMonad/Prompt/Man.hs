-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Prompt.Man
-- Description :  A manual page prompt.
-- Copyright   :  (c) 2007 Valery V. Vorotyntsev
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  Valery V. Vorotyntsev <valery.vv@gmail.com>
-- Portability :  non-portable (uses "manpath" and "bash")
--
-- A manual page prompt for XMonad window manager.
--
-- TODO
--
--   * narrow completions by section number, if the one is specified
--     (like @\/etc\/bash_completion@ does)
-----------------------------------------------------------------------------

module XMonad.Prompt.Man (
                          -- * Usage
                          -- $usage
                          manPrompt
                         , getCommandOutput
                         , Man
                         ) where


import XMonad
import XMonad.Prelude
import XMonad.Prompt
import XMonad.Util.Run
import XMonad.Prompt.Shell (split)

import System.Directory
import System.FilePath (dropExtensions, (</>))
import System.IO
import System.Process

import qualified Control.Exception as E

-- $usage
-- 1. In your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Prompt
-- > import XMonad.Prompt.Man
--
-- 2. In your keybindings add something like:
--
-- >     , ((modm, xK_F1), manPrompt def)
--
-- For detailed instruction on editing the key binding see
-- "XMonad.Doc.Extending#Editing_key_bindings".

data Man = Man

instance XPrompt Man where
    showXPrompt Man = "Manual page: "

-- | Query for manual page to be displayed.
manPrompt :: XPConfig -> X ()
manPrompt c = do
  mans <- io getMans
  mkXPrompt Man c (manCompl c mans) $ runInTerm "" . (++) "man "

getMans :: IO [String]
getMans = do
  paths <- do
    let getout cmd = getCommandOutput cmd `E.catch` \E.SomeException{} -> return ""
    -- one of these combinations should give some output
    p1 <- getout "manpath -g 2>/dev/null"
    p2 <- getout "manpath 2>/dev/null"
    return $ intercalate ":" $ lines $ p1 ++ p2
  let sects    = ["man" ++ show n | n <- [1..9 :: Int]]
      dirs     = [d </> s | d <- split ':' paths, s <- sects]
  mans <- forM (nub dirs) $ \d -> do
            exists <- doesDirectoryExist d
            if exists
              then map dropExtensions <$> getDirectoryContents d
              else return []
  return $ uniqSort $ concat mans

manCompl :: XPConfig -> [String] -> String -> IO [String]
manCompl c mans s | s == "" || last s == ' ' = return []
                  | otherwise                = do
  -- XXX readline instead of bash's compgen?
  f <- lines <$> getCommandOutput ("bash -c 'compgen -A file " ++ s ++ "'")
  mkComplFunFromList c (f ++ mans) s

-- | Run a command using shell and return its output.
--
-- XXX Merge into 'XMonad.Util.Run'?
--
-- (Ask \"gurus\" whether @evaluate (length ...)@ approach is
-- better\/more idiomatic.)
getCommandOutput :: String -> IO String
getCommandOutput s = do
  -- we can ignore the process handle because we ignor SIGCHLD
  (pin, pout, perr, _) <- runInteractiveCommand s
  hClose pin
  output <- hGetContents pout
  E.evaluate (length output)
  hClose perr
  return output
