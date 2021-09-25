-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Prompt.DirExec
-- Description :  A directory file executables prompt for XMonad.
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
    , dirExecPromptNamed
    , DirExec
    ) where

import Control.Exception as E
import System.Directory
import XMonad
import XMonad.Prelude
import XMonad.Prompt

econst :: Monad m => a -> IOException -> m a
econst = const . return

-- $usage
-- 1. In your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Prompt.DirExec
--
-- 2. In your keybindings add something like:
--
-- >   , ("M-C-x", dirExecPrompt def spawn "/home/joe/.scipts")
--
-- or
--
-- >   , ("M-C-x", dirExecPromptNamed def spawn
-- >                                  "/home/joe/.scripts" "My Scripts: ")
--
-- or add this after your default bindings:
--
-- >   ++
-- >   [ ("M-x " ++ key, dirExecPrompt def fn "/home/joe/.scripts")
-- >     | (key, fn) <- [ ("x", spawn), ("M-x", runInTerm "-hold") ]
-- >   ]
-- >   ++
--
-- The first alternative uses the last element of the directory path for
-- a name of prompt. The second alternative uses the provided string
-- for the name of the prompt. The third alternative defines 2 key bindings,
-- first one spawns the program by shell, second one runs the program in
-- terminal
--
-- For detailed instruction on editing the key binding see
-- "XMonad.Doc.Extending#Editing_key_bindings".

newtype DirExec = DirExec String

instance XPrompt DirExec where
    showXPrompt (DirExec name) = name

-- | Function 'dirExecPrompt' starts the prompt with list of all executable
-- files in directory specified by 'FilePath'. The name of the prompt is taken
-- from the last element of the path. If you specify root directory - @\/@ - as
-- the path, name @Root:@ will be used as the name of the prompt instead. The
-- 'XPConfig' parameter can be used to customize visuals of the prompt.
-- The runner parameter specifies the function used to run the program - see
-- usage for more information
dirExecPrompt :: XPConfig -> (String -> X ()) -> FilePath -> X ()
dirExecPrompt cfg runner path = do
    let name = (++ ": ") . last
                         . (["Root"] ++) -- handling of "/" path parameter
                         . words
                         . map (\x -> if x == '/' then ' ' else x)
                         $ path
    dirExecPromptNamed cfg runner path name

-- | Function 'dirExecPromptNamed' does the same as 'dirExecPrompt' except
-- the name of the prompt is specified by 'String' parameter.
dirExecPromptNamed :: XPConfig -> (String -> X ()) -> FilePath -> String -> X ()
dirExecPromptNamed cfg runner path name = do
    let path' = path ++ "/"
    cmds <- io $ getDirectoryExecutables path'
    mkXPrompt (DirExec name) cfg (compList cmds) (runner . (path' ++))
    where
        compList cmds s = return . filter (isInfixOf s) $ cmds

getDirectoryExecutables :: FilePath -> IO [String]
getDirectoryExecutables path =
    (getDirectoryContents path >>=
        filterM (\x -> let x' = path ++ x in
            liftA2 (&&)
                (doesFileExist x')
                (fmap executable (getPermissions x'))))
    `E.catch` econst []
