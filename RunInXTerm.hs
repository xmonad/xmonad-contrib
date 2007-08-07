-----------------------------------------------------------------------------
-- |
-- Module      :  XMonadContrib.RunInXTerm
-- Copyright   :  (C) 2007 Andrea Rossato
-- License     :  BSD3
-- 
-- Maintainer  :  andrea.rossato@unibz.it
-- Stability   :  unstable
-- Portability :  unportable
--
-- A simple module to launch commands in an X terminal 
-- from XMonad
--
-----------------------------------------------------------------------------

module XMonadContrib.RunInXTerm (
                             -- * Usage
                             -- $usage
                             runInXTerm
                              ) where

import XMonad
import System.Environment

-- $usage
-- For an example usage see SshPrompt

runInXTerm :: String -> X ()
runInXTerm com = do
  c <- io $ catch (getEnv "XTERMCMD") (const $ return "xterm")
  spawn ("exec " ++ c ++ " -e " ++ com)
