-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Prompt.DirExec
-- Copyright   :  (C) 2008 Juraj Hercek
-- License     :  BSD3
--
-- Maintainer  :  juhe_xmonad@hck.sk
-- Stability   :  unstable
-- Portability :  unportable
--
-- A directory file executables prompt for XMonad. This might be useful if you
-- don't want to have scripts in your PATH environment variable (same
-- executable names, different behavior) - otherwise you might want to use
-- "XMonad.Prompt.Shell" instead - but you want to have easy access to these
-- executables through the xmonad's prompt.
--
-----------------------------------------------------------------------------

module XMonad.Prompt.DirExec
    ( -- * Usage
      -- $usage
      dirExecPrompt
    , dirExecPromptWithName
    ) where

import System.Directory
import Control.Monad
import Data.List
import XMonad
import XMonad.Prompt

-- $usage
-- 1. In your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Prompt.DirExec
--
-- 2. In your keybindings add something like:
--
-- >   , ("M-C-x", dirExecPrompt defaultXPConfig "/home/joe/.scipts")
--
-- or
--
-- >   , ("M-C-x", dirExecPromptWithName defaultXPConfig "/home/joe/.scripts"
-- >                                                     "My Scripts: ")
--
-- The first alternative uses the last element of the directory path for
-- a name of prompt. The second alternative uses the provided string
-- for the name of the prompt.
--
-- For detailed instruction on editing the key binding see
-- "XMonad.Doc.Extending#Editing_key_bindings".

data DirExec = DirExec String

instance XPrompt DirExec where
    showXPrompt (DirExec name) = name

-- | Function 'dirExecPrompt' starts the prompt with list of all executable
-- files in directory specified by 'FilePath'. The name of the prompt is taken
-- from the last element of the path. If you specify root directory - @/@ - as
-- the path, name @Root:@ will be used as the name of the prompt instead. The
-- 'XPConfig' parameter can be used to customize visuals of the prompt.
dirExecPrompt :: XPConfig -> FilePath -> X ()
dirExecPrompt cfg path = do
    let name = (++ ": ") . last
                         . (["Root"] ++) -- handling of "/" path parameter
                         . words
                         . map (\x -> if x == '/' then ' ' else x)
                         $ path
    dirExecPromptWithName cfg path name

-- | Function 'dirExecPromptWithName' does the same as 'dirExecPrompt' except
-- the name of the prompt is specified by 'String' parameter.
dirExecPromptWithName :: XPConfig -> FilePath -> String -> X ()
dirExecPromptWithName cfg path name = do
    let path' = path ++ "/"
    cmds <- io $ getDirectoryExecutables path'
    mkXPrompt (DirExec name) cfg (compList cmds) (spawn . (path' ++))
    where
        compList cmds s = return . filter (isInfixOf s) $ cmds

getDirectoryExecutables :: FilePath -> IO [String]
getDirectoryExecutables path =
    (getDirectoryContents path >>=
        filterM (\x -> let x' = path ++ x in
            liftM2 (&&)
                (doesFileExist x')
                (liftM executable (getPermissions x'))))
    `catch` (return . return . show)

