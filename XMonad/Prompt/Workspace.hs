-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Prompt.Workspace
-- Copyright   :  (C) 2007 Andrea Rossato, David Roundy
-- License     :  BSD3
-- 
-- Maintainer  :  droundy@darcs.net
-- Stability   :  unstable
-- Portability :  unportable
--
-- A workspace prompt for XMonad
--
-----------------------------------------------------------------------------

module XMonad.Prompt.Workspace (
                             -- * Usage
                             -- $usage
                             workspacePrompt
                              ) where

import Control.Monad.State ( gets )
import Data.List ( sort )
import XMonad hiding ( workspaces )
import XMonad.Prompt
import XMonad.StackSet ( workspaces, tag )

-- $usage
-- You can use this module with the following in your Config.hs file:
-- 
-- > import XMonad.Prompt
-- > import XMonad.Prompt.Workspace
--
-- >   , ((modMask .|. shiftMask, xK_m     ), workspacePrompt defaultXPConfig (windows . W.shift))

data Wor = Wor String

instance XPrompt Wor where
    showXPrompt (Wor x) = x

workspacePrompt :: XPConfig -> (String -> X ()) -> X ()
workspacePrompt c job = do ws <- gets (workspaces . windowset)
                           let ts = sort $ map tag ws
                           mkXPrompt (Wor "") c (mkCompl ts) job

mkCompl :: [String] -> String -> IO [String]
mkCompl l s = return $ filter (\x -> take (length s) x == s) l
