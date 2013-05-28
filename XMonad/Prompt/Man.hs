-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Prompt.Man
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
import XMonad.Prompt
import XMonad.Util.Run
import XMonad.Prompt.Shell (split)

import System.Directory
import System.Process
import System.IO

import qualified Control.Exception.Extensible as E
import Control.Monad
import Data.List
import Data.Maybe

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
  mkXPrompt Man c (manCompl mans) $ runInTerm "" . (++) "man "

getMans :: IO [String]
getMans = do
  paths <- getCommandOutput "manpath -g 2>/dev/null" `E.catch`
            \(E.SomeException _) -> return []
  let sects    = ["man" ++ show n | n <- [1..9 :: Int]]
      dirs     = [d ++ "/" ++ s | d <- split ':' paths, s <- sects]
  mans <- forM dirs $ \d -> do
            exists <- doesDirectoryExist d
            if exists
              then map (stripExt . stripSuffixes [".gz", ".bz2"]) `fmap`
                   getDirectoryContents d
              else return []
  return $ uniqSort $ concat mans

manCompl :: [String] -> String -> IO [String]
manCompl mans s | s == "" || last s == ' ' = return []
                | otherwise                = do
  -- XXX readline instead of bash's compgen?
  f <- lines `fmap` getCommandOutput ("bash -c 'compgen -A file " ++ s ++ "'")
  mkComplFunFromList (f ++ mans) s

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

stripExt :: String -> String
stripExt = reverse . drop 1 . dropWhile (/= '.') . reverse

stripSuffixes :: Eq a => [[a]] -> [a] -> [a]
stripSuffixes sufs fn =
    head . catMaybes $ map (flip rstrip fn) sufs ++ [Just fn]

rstrip :: Eq a => [a] -> [a] -> Maybe [a]
rstrip suf lst
    | suf `isSuffixOf` lst = Just $ take (length lst - length suf) lst
    | otherwise            = Nothing
