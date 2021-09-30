-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Prompt.AppLauncher
-- Description :  A prompt for launch applications that receive command line parameters.
-- Copyright   :  (C) 2008 Luis Cabellos
-- License     :  BSD3
--
-- Maintainer  :  zhen.sydow@gmail.com
-- Stability   :  unstable
-- Portability :  unportable
--
-- A module for launch applicationes that receive parameters in the command
-- line. The launcher call a prompt to get the parameters.
--
-----------------------------------------------------------------------------
module XMonad.Prompt.AppLauncher ( -- * Usage
                                    -- $usage
                                    launchApp
                                   ,module XMonad.Prompt
                                  -- * Use case: launching gimp with file
                                  -- $tip

                                  -- * Types
                                   ,Application, AppPrompt,
                                  ) where

import XMonad (X(),MonadIO)
import XMonad.Core (spawn)
import XMonad.Prompt (XPrompt(showXPrompt), mkXPrompt, XPConfig(searchPredicate))
import XMonad.Prompt.Shell (getShellCompl)

{- $usage
   This module is intended to allow the launch of the same application
   but changing the parameters using the user response. For example, when
   you want to open a image in gimp program, you can open gimp and then use
   the File Menu to open the image or you can use this module to select
   the image in the command line.

   We use Prompt to get the user command line. This also allow to autoexpand
   the names of the files when we are writing the command line.
 -}

{- $tip

First, you need to import necessary modules. Prompt is used to get the promp
configuration and the AppLauncher module itself.

> import XMonad.Prompt
> import XMonad.Prompt.AppLauncher as AL

Then you can add the bindings to the applications.

> ...
> , ((modm, xK_g), AL.launchApp def "gimp" )
> , ((modm, xK_g), AL.launchApp def "evince" )
> ...

 -}

-- A customized prompt
newtype AppPrompt = AppPrompt String
instance XPrompt AppPrompt where
    showXPrompt (AppPrompt n) = n ++ " "

type Application = String
type Parameters = String

{- | Given an application and its parameters, launch the application. -}
launch :: MonadIO m => Application -> Parameters -> m ()
launch app params = spawn ( app ++ " " ++ params )


{- | Get the user's response to a prompt an launch an application using the
   input as command parameters of the application.-}
launchApp :: XPConfig -> Application -> X ()
launchApp config app = mkXPrompt (AppPrompt app) config (getShellCompl [] $ searchPredicate config) $ launch app
