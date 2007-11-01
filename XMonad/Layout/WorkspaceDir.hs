{-# OPTIONS_GHC -fglasgow-exts #-} -- For deriving Data/Typeable
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.WorkspaceDir
-- Copyright   :  (c) 2007  David Roundy <droundy@darcs.net>
-- License     :  BSD3-style (see LICENSE)
-- 
-- Maintainer  :  David Roundy <droundy@darcs.net>
-- Stability   :  unstable
-- Portability :  unportable
--
-- WorkspaceDir is an extension to set the current directory in a workspace.
--
-- Actually, it sets the current directory in a layout, since there's no way I
-- know of to attach a behavior to a workspace.  This means that any terminals
-- (or other programs) pulled up in that workspace (with that layout) will
-- execute in that working directory.  Sort of handy, I think.
-- 
-- Requires the 'directory' package
--
-----------------------------------------------------------------------------

module XMonad.Layout.WorkspaceDir ( 
                                   -- * Usage
                                   -- $usage
                                   workspaceDir, 
                                   changeDir
                                  ) where

import System.Directory ( setCurrentDirectory, getCurrentDirectory )

import XMonad
import XMonad.Operations ( sendMessage )
import XMonad.Util.Run ( runProcessWithInput )
import XMonad.Prompt ( XPConfig )
import XMonad.Prompt.Directory ( directoryPrompt )
import XMonad.Layout.LayoutModifier

-- $usage
-- You can use this module with the following in your Config.hs file:
--
-- > import XMonad.Layout.WorkspaceDir
-- >
-- > layouts = map (workspaceDir "~") [ tiled, ... ]
--
-- In keybindings:
--
-- >  , ((modMask .|. shiftMask, xK_x     ), changeDir defaultXPConfig)

-- %import XMonad.Layout.WorkspaceDir
-- %keybind , ((modMask .|. shiftMask, xK_x     ), changeDir defaultXPConfig)
-- %layout -- prepend 'map (workspaceDir "~")' to layouts definition above,
-- %layout -- just before the list, like the following (don't uncomment next line):
-- %layout -- layouts = map (workspaceDir "~") [ tiled, ... ]


data Chdir = Chdir String deriving ( Typeable )
instance Message Chdir

data WorkspaceDir a = WorkspaceDir String deriving ( Read, Show )

instance LayoutModifier WorkspaceDir a where
    hook (WorkspaceDir s) = scd s
    handleMess (WorkspaceDir _) m
        | Just (Chdir wd) <- fromMessage m = do wd' <- cleanDir wd
                                                return $ Just $ WorkspaceDir wd'
        | otherwise = return Nothing

workspaceDir :: LayoutClass l a => String -> l a
             -> ModifiedLayout WorkspaceDir l a
workspaceDir s = ModifiedLayout (WorkspaceDir s)

cleanDir :: String -> X String
cleanDir x = scd x >> io getCurrentDirectory

scd :: String -> X ()
scd x = do x' <- io (runProcessWithInput "bash" [] ("echo -n " ++ x) `catch` \_ -> return x)
           catchIO $ setCurrentDirectory x'

changeDir :: XPConfig -> X ()
changeDir c = directoryPrompt c "Set working directory: " (sendMessage . Chdir)
