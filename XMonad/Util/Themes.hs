-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Util.Themes
-- Copyright   :  (C) 2007 Andrea Rossato
-- License     :  BSD3
--
-- Maintainer  :  andrea.rossato@unibz.it
-- Stability   :  unstable
-- Portability :  unportable
--
-- A (hopefully) growing collection of themes for xmonad
--
-----------------------------------------------------------------------------

module XMonad.Util.Themes
    ( -- * Usage
      -- $usage
      listOfThemes
    , xmonadTheme
    , smallClean
    , ThemeInfo (..)
    ) where

import XMonad.Layout.Decoration

-- $usage
-- This module stores some user contributed themes. If you have a theme
-- you would like to share, adding it to this module is very easy.
--
-- You can use 'xmonadTheme' or 'smallClean' as a template.
--
-- At the present time only the 'themeName' field is used. But please
-- provide all the other information, which will be used at a later
-- time.
--
-- Please, remember to add your theme to the list of exported
-- functions, and to the 'listOfThemes'.
--
-- Thanks for your contribution!

data ThemeInfo =
    TI { themeName        :: String
       , themeAuthor      :: String
       , themeDescription :: String
       , theme            :: Theme
    }

newTheme :: ThemeInfo
newTheme = TI "" "" "" defaultTheme

listOfThemes :: [ThemeInfo]
listOfThemes = [ xmonadTheme
               , smallClean
               , deiflTheme
               ]

xmonadTheme :: ThemeInfo
xmonadTheme =
    newTheme { themeName        = "xmonadTheme"
             , themeAuthor      = "David Roundy"
             , themeDescription = "The default xmonad theme"
             , theme            = defaultTheme
             }

smallClean :: ThemeInfo
smallClean =
    newTheme { themeName        = "smallClean"
             , themeAuthor      = "Andrea Rossato"
             , themeDescription = "Small decorations with a Ion3 remembrance"
             , theme            = defaultTheme { activeColor         = "#8a999e"
                                               , inactiveColor       = "#545d75"
                                               , activeBorderColor   = "white"
                                               , inactiveBorderColor = "grey"
                                               , activeTextColor     = "white"
                                               , inactiveTextColor   = "grey"
                                               , decoHeight          = 14
                                               }
             }

deiflTheme :: ThemeInfo
deiflTheme =
    newTheme { themeName        = "deiflTheme"
             , themeAuthor      = "deiflTheme"
             , themeDescription = "deiflTheme"
             , theme            = defaultTheme { inactiveBorderColor = "#708090"
                                               , activeBorderColor   = "#5f9ea0"
                                               , activeColor         = "#000000"
                                               , inactiveColor       = "#333333"
                                               , inactiveTextColor   = "#888888"
                                               , activeTextColor     = "#87cefa"
                                               , fontName            = "-xos4-terminus-*-*-*-*-12-*-*-*-*-*-*-*"
                                               , decoHeight          = 15
                                               }
             }
