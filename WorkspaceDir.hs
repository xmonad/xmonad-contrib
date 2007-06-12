{-# OPTIONS -fglasgow-exts #-}
module XMonadContrib.WorkspaceDir ( workspaceDir, changeDir ) where

-- to use:

-- import XMonadContrib.WorkspaceDir

-- defaultLayouts = map (workspaceDir "~") [ tiled, ... ]

-- In keybindings:
--  , ((modMask .|. shiftMask, xK_x     ), changeDir ["~","/tmp"])

import System.Directory ( setCurrentDirectory, getCurrentDirectory )
import Data.List ( nub )

import XMonad
import Operations ( sendMessage )
import XMonadContrib.Dmenu ( dmenu, runProcessWithInput )

data Chdir = Chdir String deriving ( Typeable )
instance Message Chdir

workspaceDir :: String -> Layout -> Layout
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
