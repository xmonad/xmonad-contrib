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
-- A (hopefully) growing collection of themes for decorated layouts.
--
-----------------------------------------------------------------------------

module XMonad.Util.Themes
    ( -- * Usage
      -- $usage
      listOfThemes
    , ppThemeInfo
    , xmonadTheme
    , smallClean
    , robertTheme
    , deiflTheme
    , oxymor00nTheme
    , donaldTheme
    , wfarrTheme
    , ThemeInfo (..)
    ) where

import XMonad.Layout.Decoration

-- $usage
-- This module stores some user contributed themes which can be used
-- with decorated layouts (such as Tabbed).  (Note that these themes
-- only apply to decorated layouts, such as those found in
-- "XMonad.Layout.Tabbed" and "XMonad.Layout.DecorationMadness"; they
-- do not apply to xmonad as a whole.)
--
-- If you want to use one of them with one of your decorated layouts,
-- you need to substitute defaultTheme with, for instance, (theme
-- smallClean).
--
-- Here is an example:
--
-- > import XMonad
-- > import XMonad.Util.Themes
-- > import XMonad.Layout.Tabbed
-- >
-- > myLayout = tabbed shrinkText (theme smallClean)
-- >
-- > main = xmonad defaultConfig {layoutHook = myLayout}
--
-- If you have a theme you would like to share, adding it to this
-- module is very easy.
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

ppThemeInfo :: ThemeInfo -> String
ppThemeInfo t = themeName t <> themeDescription t <> "by" <> themeAuthor t
    where "" <> x = x
          x <> y = x ++ " - " ++ y


listOfThemes :: [ThemeInfo]
listOfThemes = [ xmonadTheme
               , smallClean
               , deiflTheme
               , oxymor00nTheme
               , robertTheme
               , donaldTheme
               , wfarrTheme
               ]

-- | The default xmonad theme, by David Roundy.
xmonadTheme :: ThemeInfo
xmonadTheme =
    newTheme { themeName        = "xmonadTheme"
             , themeAuthor      = "David Roundy"
             , themeDescription = "The default xmonad theme"
             , theme            = defaultTheme
             }

-- | Small decorations with a Ion3 remembrance, by Andrea Rossato.
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

-- | Don's prefered colors - from DynamicLog...;)
donaldTheme  :: ThemeInfo
donaldTheme =
    newTheme { themeName        = "donaldTheme"
             , themeAuthor      = "Andrea Rossato"
             , themeDescription = "Don's prefered colors - from DynamicLog...;)"
             , theme            = defaultTheme { activeColor         = "#2b4f98"
                                               , inactiveColor       = "#cccccc"
                                               , activeBorderColor   = "#2b4f98"
                                               , inactiveBorderColor = "#cccccc"
                                               , activeTextColor     = "white"
                                               , inactiveTextColor   = "black"
                                               , decoHeight          = 16
                                               }
             }

-- | Ffrom Robert Manea's prompt theme.
robertTheme  :: ThemeInfo
robertTheme =
    newTheme { themeName        = "robertTheme"
             , themeAuthor      = "Andrea Rossato"
             , themeDescription = "From Robert Manea's prompt theme"
             , theme            = defaultTheme { activeColor         = "#aecf96"
                                               , inactiveColor       = "#111111"
                                               , activeBorderColor   = "#aecf96"
                                               , inactiveBorderColor = "#111111"
                                               , activeTextColor     = "black"
                                               , inactiveTextColor   = "#d5d3a7"
                                               , fontName            = "-*-profont-*-*-*-*-11-*-*-*-*-*-iso8859"
                                               , decoHeight          = 16
                                               }
             }

-- | deifl\'s Theme, by deifl.
deiflTheme :: ThemeInfo
deiflTheme =
    newTheme { themeName        = "deiflTheme"
             , themeAuthor      = "deifl"
             , themeDescription = "deifl's Theme"
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

-- | oxymor00n\'s theme, by Tom Rauchenwald.
oxymor00nTheme :: ThemeInfo
oxymor00nTheme =
    newTheme { themeName        = "oxymor00nTheme"
             , themeAuthor      = "Tom Rauchenwald"
             , themeDescription = "oxymor00n's theme"
             , theme            = defaultTheme { inactiveBorderColor = "#000"
                                               , activeBorderColor = "aquamarine3"
                                               , activeColor = "aquamarine3"
                                               , inactiveColor = "DarkSlateGray4"
                                               , inactiveTextColor = "#222"
                                               , activeTextColor = "#222"
                                               -- This font can be found in the package ttf-alee
                                               -- on debian-systems
                                               , fontName = "-*-Bandal-*-*-*-*-12-*-*-*-*-*-*-*"
                                               , decoHeight = 15
                                               , urgentColor = "#000"
                                               , urgentTextColor = "#63b8ff"
                                               }
             }

wfarrTheme :: ThemeInfo
wfarrTheme =
    newTheme { themeName        = "wfarrTheme"
             , themeAuthor      = "Will Farrington"
             , themeDescription = "A nice blue/black theme."
             , theme            = defaultTheme { activeColor         = "#4c7899"
                                               , inactiveColor       = "#333333"
                                               , activeBorderColor   = "#285577"
                                               , inactiveBorderColor = "#222222"
                                               , activeTextColor     = "#ffffff"
                                               , inactiveTextColor   = "#888888"
                                               , fontName            = "-*-fixed-medium-r-*--10-*-*-*-*-*-iso8859-1"
                                               , decoHeight          = 12
                                               }
             }
