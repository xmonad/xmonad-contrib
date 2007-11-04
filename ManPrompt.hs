{-# OPTIONS_GHC -Wall #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  XMonadContrib.ManPrompt
-- Copyright   :  (c) 2007 Valery V. Vorotyntsev
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  valery.vv@gmail.com
-- Stability   :  unstable
-- Portability :  non-portable (uses \"manpath\" and \"bash\")
--
-- A manual page prompt for XMonad window manager.
--
-- TODO
--
--   * narrow completions by section number, if the one is specified
--     (like @\/etc\/bash_completion@ does)
--
--   * test with QuickCheck
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
-- >     , ((modMask, xK_F1), manPrompt defaultXPConfig) -- mod-f1 %! Query for manual page to be displayed

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
manCompl str | '/' `elem` str = do
  -- XXX It may be better to use readline instead of bash's compgen...
  lines `fmap` getCommandOutput ("bash -c 'compgen -A file " ++ str ++ "'")
             | otherwise      = do
  mp <- getCommandOutput "manpath -g 2>/dev/null" `E.catch` \_ -> return []
  let sects    = ["man" ++ show n | n <- [1..9 :: Int]]
      dirs     = [d ++ "/" ++ s | d <- split ':' mp, s <- sects]
      stripExt = reverse . drop 1 . dropWhile (/= '.') . reverse
  mans <- forM dirs $ \d -> do
            exists <- doesDirectoryExist d
            if exists
              then map (stripExt . stripSuffixes [".gz", ".bz2"]) `fmap`
                   getDirectoryContents d
              else return []
  mkComplFunFromList (uniqSort $ concat mans) str

-- | Run a command using shell and return its output.
--
-- XXX merge with 'Run.runProcessWithInput'?
--
--   * update documentation of the latter (there is no 'Maybe' in result)
--
--   * ask \"gurus\" whether @evaluate (length ...)@ approach is
--     better\/more idiomatic
getCommandOutput :: String -> IO String
getCommandOutput s = do
  (pin, pout, perr, ph) <- runInteractiveCommand s
  hClose pin
  output <- hGetContents pout
  E.evaluate (length output)
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
