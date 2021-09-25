{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, PatternGuards #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.WorkspaceDir
-- Description :  A layout modifier to set the current directory in a workspace.
-- Copyright   :  (c) 2007  David Roundy <droundy@darcs.net>
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  none
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
-- Note this extension requires the 'directory' package to be installed.
--
-----------------------------------------------------------------------------

module XMonad.Layout.WorkspaceDir (
                                   -- * Usage
                                   -- $usage
                                   workspaceDir,
                                   changeDir,
                                   WorkspaceDir,
                                   Chdir(Chdir),
                                  ) where

import System.Directory ( setCurrentDirectory, getCurrentDirectory )
import XMonad.Prelude ( when )

import XMonad hiding ( focus )
import XMonad.Prompt ( XPConfig )
import XMonad.Prompt.Directory ( directoryPrompt )
import XMonad.Layout.LayoutModifier
import XMonad.StackSet ( tag, currentTag )

-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Layout.WorkspaceDir
--
-- Then edit your @layoutHook@ by adding the Workspace layout modifier
-- to some layout:
--
-- > myLayout = workspaceDir "~" (Tall 1 (3/100) (1/2))  ||| Full ||| etc..
-- > main = xmonad def { layoutHook = myLayout }
--
-- For more detailed instructions on editing the layoutHook see:
--
-- "XMonad.Doc.Extending#Editing_the_layout_hook"
--
-- WorkspaceDir provides also a prompt. To use it you need to import
-- "XMonad.Prompt" and add something like this to your key bindings:
--
-- >  , ((modm .|. shiftMask, xK_x     ), changeDir def)
--
-- If you prefer a prompt with case-insensitive completion:
--
-- >  , ((modm .|. shiftMask, xK_x     ),
--       changeDir def {complCaseSensitivity = CaseInSensitive})
--
-- For detailed instruction on editing the key binding see:
--
-- "XMonad.Doc.Extending#Editing_key_bindings".

newtype Chdir = Chdir String
instance Message Chdir

newtype WorkspaceDir a = WorkspaceDir String deriving ( Read, Show )

instance LayoutModifier WorkspaceDir Window where
    modifyLayout (WorkspaceDir d) w r = do tc <- gets (currentTag.windowset)
                                           when (tc == tag w) $ scd d
                                           runLayout w r
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
scd x = catchIO $ setCurrentDirectory x

changeDir :: XPConfig -> X ()
changeDir c = directoryPrompt c "Set working directory: " (sendMessage . Chdir)
