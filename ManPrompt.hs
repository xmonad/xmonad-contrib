-----------------------------------------------------------------------------
-- |
-- Module      :  XMonadContrib.ManPrompt
-- Copyright   :  (c) 2007 Valery V. Vorotyntsev
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  valery.vv@gmail.com
-- Stability   :  unstable
-- Portability :  unportable
--
-- A manual page prompt for XMonad window manager.
--
-- TODO
--
--   * narrow completions by section number, if the one is specified
--     (like @\/etc\/bash_completion@ does)
--
--   * handle explicit paths (e.g., @~\/src\/xmonad\/man\/xmonad.1@)
--
--   * quickcheck properties
-----------------------------------------------------------------------------

module XMonadContrib.ManPrompt (
                                -- * Usage
                                -- $usage
                                manPrompt
                               , getCommandOutput
                               ) where

import XMonad
import XMonadContrib.XPrompt
import XMonadContrib.Run
import XMonadContrib.ShellPrompt (split)

import System.Directory
import System.Process
import System.IO

import qualified Control.Exception as E
import Control.Monad
import Data.List
import Data.Maybe

-- $usage
-- 1. In Config.hs add:
--
-- > import XMonadContrib.ManPrompt
--
-- 2. In your keybindings add something like:
--
-- >     , ((modMask, xK_F1), manPrompt defaultXPConfig)

-- %import XMonadContrib.XPrompt
-- %import XMonadContrib.ManPrompt
-- %keybind , ((modMask, xK_F1), manPrompt defaultXPConfig)

data Man = Man

instance XPrompt Man where
    showXPrompt Man = "Manual page: "

-- | Query for manual page to be displayed.
manPrompt :: XPConfig -> X ()
manPrompt c = mkXPrompt Man c manCompl $ runInTerm . (++) "man "

manCompl :: String -> IO [String]
manCompl s = getManpages >>= flip mkComplFunFromList s

-- | Obtain the list of manual pages.
--
-- /XXX Code duplication!/
-- Adopted from 'ShellPrompt.getCommands'.
getManpages :: IO [String]
getManpages = do
  p <- getCommandOutput "manpath -g 2>/dev/null" `E.catch` const (return [])
  let sections = ["man" ++ show n | n <- [1..9 :: Int]] -- XXX "cat1".."cat9"?
      ds       = [d ++ "/" ++ s | d <- split ':' p, s <- sections]
      stripSec = reverse . drop 1 . dropWhile (/= '.') . reverse
  ms <- forM ds $ \d -> do
              exists <- doesDirectoryExist d
              if exists
                then map (stripSec . stripSuffixes [".gz", ".bz2"]) `fmap`
                     getDirectoryContents d
                else return []
  return . uniqSort . concat $ ms

-- | Run a command using shell and return its output.
getCommandOutput :: String -> IO String
getCommandOutput s = do
  (pin, pout, perr, ph) <- runInteractiveCommand s
  hClose pin
  output <- hGetContents pout
  E.evaluate (null output)
  hClose perr
  waitForProcess ph
  return output

stripSuffixes :: Eq a => [[a]] -> [a] -> [a]
stripSuffixes sufs fn =
    head . catMaybes $ map (flip rstrip fn) sufs ++ [Just fn]

rstrip :: Eq a => [a] -> [a] -> Maybe [a]
rstrip suf lst
    | suf `isSuffixOf` lst = Just $ take (length lst - length suf) lst
    | otherwise            = Nothing
