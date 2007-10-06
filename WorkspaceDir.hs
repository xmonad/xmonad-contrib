{-# LANGUAGE FlexibleInstances #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  XMonadContrib.WorkspaceDir
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
-----------------------------------------------------------------------------

module XMonadContrib.WorkspaceDir ( 
                                   -- * Usage
                                   -- $usage
                                   workspaceDir, 
                                   changeDir
                                  ) where

import System.Directory ( setCurrentDirectory )

import XMonad
import Operations ( sendMessage )
import XMonadContrib.Dmenu ( runProcessWithInput )
import XMonadContrib.XPrompt ( XPConfig )
import XMonadContrib.DirectoryPrompt ( directoryPrompt )
import XMonadContrib.LayoutModifier

-- $usage
-- You can use this module with the following in your Config.hs file:
--
-- > import XMonadContrib.WorkspaceDir
-- >
-- > defaultLayouts = map (workspaceDir "~") [ tiled, ... ]
--
-- In keybindings:
--
-- >  , ((modMask .|. shiftMask, xK_x     ), changeDir defaultXPConfig)

-- %import XMonadContrib.WorkspaceDir
-- %keybind , ((modMask .|. shiftMask, xK_x     ), changeDir defaultXPConfig)
-- %layout -- prepend 'map (workspaceDir "~")' to defaultLayouts definition above,
-- %layout -- just before the list, like the following (don't uncomment next line):
-- %layout -- defaultLayouts = map (workspaceDir "~") [ tiled, ... ]


data Chdir = Chdir String deriving ( Typeable )
instance Message Chdir

data WorkspaceDir a = WorkspaceDir String deriving ( Read, Show )

instance LayoutModifier WorkspaceDir a where
    hook (WorkspaceDir s) = scd s
    handleMess (WorkspaceDir _) m = return $ do Chdir wd <- fromMessage m
                                                Just (WorkspaceDir wd)

workspaceDir :: LayoutClass l a => String -> l a
             -> ModifiedLayout WorkspaceDir l a
workspaceDir s = ModifiedLayout (WorkspaceDir s)

scd :: String -> X ()
scd x = do x' <- io (runProcessWithInput "bash" [] ("echo -n " ++ x) `catch` \_ -> return Nothing)
           case x' of
             Just newDir -> catchIO $ setCurrentDirectory newDir
             Nothing     -> return ()

changeDir :: XPConfig -> X ()
changeDir c = directoryPrompt c "Set working directory: " (sendMessage . Chdir)
