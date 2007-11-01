-----------------------------------------------------------------------------
-- |
-- Module       : XMonad.Actions.WmiiActions
-- Copyright    : (c) Juraj Hercek <juhe_xmonad@hck.sk>
-- License      : BSD3
--
-- Maintainer   : Juraj Hercek <juhe_xmonad@hck.sk>
-- Stability    : unstable
-- Portability  : unportable
--
-- Provides `actions' as known from Wmii window manager (
-- <http://wmii.suckless.org>). It also provides slightly better interface for
-- running dmenu on xinerama screens. If you want to use xinerama functions,
-- you have to apply following patch (see Dmenu.hs extension):
-- <http://www.jcreigh.com/dmenu/dmenu-3.2-xinerama.patch>.  Don't forget to
-- recompile dmenu afterwards ;-).
-----------------------------------------------------------------------------

module XMonad.Actions.WmiiActions (
                                 -- * Usage
                                 -- $usage
                                   wmiiActions
                                 , wmiiActionsXinerama
                                 , executables
                                 , executablesXinerama
                                 ) where

import XMonad
import XMonad.Util.Dmenu (dmenu, dmenuXinerama)
import XMonad.Util.Run (runProcessWithInput)

import Control.Monad (filterM, liftM, liftM2)
import System.Directory (getDirectoryContents, doesFileExist, getPermissions, executable)

-- $usage
--
-- You can use this module with the following in your Config.hs file:
--
-- > import XMonad.Actions.WmiiActions
--
--  and add following to the list of keyboard bindings:
--
-- > ,((modMask, xK_a), wmiiActions "/home/joe/.wmii-3.5/")
--
-- or, if you are using xinerama, you can use
--
-- > ,((modMask, xK_a), wmiiActionsXinerama "/home/joe/.wmii-3.5/")
--
-- however, make sure you have also xinerama build of dmenu (for more
-- information see "XMonad.Util.Dmenu" extension).

-- | The 'wmiiActions' function takes the file path as a first argument and
-- executes dmenu with all executables found in the provided path.
wmiiActions :: FilePath -> X ()
wmiiActions path =
        wmiiActionsDmenu path dmenu

-- | The 'wmiiActionsXinerama'  does the same as 'wmiiActions', but it shows
-- dmenu only on workspace which currently owns focus.
wmiiActionsXinerama :: FilePath -> X ()
wmiiActionsXinerama path =
        wmiiActionsDmenu path dmenuXinerama

wmiiActionsDmenu :: FilePath -> ([String] -> X String) -> X ()
wmiiActionsDmenu path dmenuBrand =
        let path' = path ++ "/" in
        getExecutableFileList path' >>= dmenuBrand >>= spawn . (path' ++)

getExecutableFileList :: FilePath -> X [String]
getExecutableFileList path =
        io $ getDirectoryContents path >>=
             filterM (\x -> let x' = path ++ x in
                            liftM2 (&&)
                              (doesFileExist x')
                              (liftM executable (getPermissions x')))

{-
getExecutableFileList :: FilePath -> X [String]
getExecutableFileList path =
        io $ getDirectoryContents path >>=
             filterM (doesFileExist . (path ++)) >>=
             filterM (liftM executable . getPermissions . (path ++))
-}

-- | The 'executables' function runs dmenu_path script providing list of
-- executable files accessible from $PATH variable.
executables :: X ()
executables = executablesDmenu dmenu

-- | The 'executablesXinerama' function does the same as 'executables' function
-- but on workspace which currently owns focus.
executablesXinerama :: X ()
executablesXinerama = executablesDmenu dmenuXinerama

executablesDmenu :: ([String] -> X String) -> X ()
executablesDmenu dmenuBrand =
        getExecutablesList >>= dmenuBrand >>= spawn

getExecutablesList :: X [String]
getExecutablesList =
        io $ liftM lines $ runProcessWithInput "dmenu_path" [] ""

