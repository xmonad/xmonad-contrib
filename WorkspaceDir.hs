{-# OPTIONS -fglasgow-exts #-}
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
-- WorkspaceDir is an exstension to set the current directory in a workspace.
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

import System.Directory ( setCurrentDirectory, getCurrentDirectory )
import Data.List ( nub )

import XMonad
import Operations ( sendMessage )
import XMonadContrib.Dmenu ( dmenu, runProcessWithInput )

-- $usage
-- You can use this module with the following in your Config.hs file:
--
-- > import XMonadContrib.WorkspaceDir
-- >
-- > defaultLayouts = map (workspaceDir "~") [ tiled, ... ]
--
-- In keybindings:
--
-- >  , ((modMask .|. shiftMask, xK_x     ), changeDir ["~","/tmp"])


data Chdir = Chdir String deriving ( Typeable )
instance Message Chdir

workspaceDir :: String -> Layout a -> Layout a
workspaceDir wd l = l { doLayout = \r x -> scd wd >> doLayout l r x
                      , modifyLayout = ml }
    where ml m | Just (Chdir wd') <- fromMessage m = return $ Just (workspaceDir wd' l)
               | otherwise = fmap (workspaceDir wd) `fmap` modifyLayout l m

scd :: String -> X ()
scd x = do x' <- io (runProcessWithInput "bash" [] ("echo -n " ++ x) `catch` \_ -> return x)
           catchIO $ setCurrentDirectory x'

changeDir :: [String] -> X ()
changeDir dirs = do thisd <- io getCurrentDirectory
                    dir <- dmenu (nub (thisd:dirs))
                    sendMessage (Chdir dir)
