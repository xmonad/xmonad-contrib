-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Util.Themes
-- Description :  A collection of themes for decorated layouts.
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
    , adwaitaTheme
    , adwaitaDarkTheme
    , robertTheme
    , darkTheme
    , deiflTheme
    , oxymor00nTheme
    , donaldTheme
    , wfarrTheme
    , kavonForestTheme
    , kavonLakeTheme
    , kavonPeacockTheme
    , kavonVioGreenTheme
    , kavonBluesTheme
    , kavonAutumnTheme
    , kavonFireTheme
    , kavonChristmasTheme
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
-- you need to substitute def with, for instance, (theme smallClean).
--
-- Here is an example:
--
-- > import XMonad
-- > import XMonad.Util.Themes
-- > import XMonad.Layout.Tabbed
-- >
-- > myLayout = tabbed shrinkText (theme smallClean)
-- >
-- > main = xmonad def {layoutHook = myLayout}
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
newTheme = TI "" "" "" def

ppThemeInfo :: ThemeInfo -> String
ppThemeInfo t = themeName t `add` themeDescription t `add` "by" `add` themeAuthor t
    where "" `add` x = x
          x `add` y = x ++ " - " ++ y


listOfThemes :: [ThemeInfo]
listOfThemes = [ xmonadTheme
               , smallClean
               , adwaitaTheme
               , adwaitaDarkTheme
               , darkTheme
               , deiflTheme
               , oxymor00nTheme
               , robertTheme
               , donaldTheme
               , wfarrTheme
               , kavonForestTheme
               , kavonLakeTheme
               , kavonPeacockTheme
               , kavonVioGreenTheme
               , kavonBluesTheme
               , kavonAutumnTheme
               , kavonFireTheme
               , kavonChristmasTheme
               ]

-- | The default xmonad theme, by David Roundy.
xmonadTheme :: ThemeInfo
xmonadTheme =
    newTheme { themeName        = "xmonadTheme"
             , themeAuthor      = "David Roundy"
             , themeDescription = "The default xmonad theme"
             , theme            = def
             }

-- | Small decorations with a Ion3 remembrance, by Andrea Rossato.
smallClean :: ThemeInfo
smallClean =
    newTheme { themeName        = "smallClean"
             , themeAuthor      = "Andrea Rossato"
             , themeDescription = "Small decorations with a Ion3 remembrance"
             , theme            = def { activeColor         = "#8a999e"
                                      , inactiveColor       = "#545d75"
                                      , activeBorderColor   = "white"
                                      , inactiveBorderColor = "grey"
                                      , activeTextColor     = "white"
                                      , inactiveTextColor   = "grey"
                                      , decoHeight          = 14
                                      }
             }

-- | Matching decorations for Adwaita GTK theme
adwaitaTheme :: ThemeInfo
adwaitaTheme =
    newTheme { themeName        = "adwaitaTheme"
             , themeAuthor      = "Alex Griffin"
             , themeDescription = "Matching decorations for Adwaita GTK theme"
             , theme            = def { activeColor         = "#dfdcd8"
                                      , inactiveColor       = "#f6f5f4"
                                      , urgentColor         = "#3584e4"
                                      , activeBorderColor   = "#bfb8b1"
                                      , inactiveBorderColor = "#cdc7c2"
                                      , urgentBorderColor   = "#1658a7"
                                      , activeTextColor     = "#2e3436"
                                      , inactiveTextColor   = "#929595"
                                      , urgentTextColor     = "#ffffff"
                                      , fontName            = "xft:Cantarell:bold:size=11"
                                      , decoWidth           = 400
                                      , decoHeight          = 35
                                      }
             }

-- | Matching decorations for Adwaita-dark GTK theme
adwaitaDarkTheme :: ThemeInfo
adwaitaDarkTheme =
    newTheme { themeName        = "adwaitaDarkTheme"
             , themeAuthor      = "Alex Griffin"
             , themeDescription = "Matching decorations for Adwaita-dark GTK theme"
             , theme            = def { activeColor         = "#2d2d2d"
                                      , inactiveColor       = "#353535"
                                      , urgentColor         = "#15539e"
                                      , activeBorderColor   = "#070707"
                                      , inactiveBorderColor = "#1c1c1c"
                                      , urgentBorderColor   = "#030c17"
                                      , activeTextColor     = "#eeeeec"
                                      , inactiveTextColor   = "#929291"
                                      , urgentTextColor     = "#ffffff"
                                      , fontName            = "xft:Cantarell:bold:size=11"
                                      , decoWidth           = 400
                                      , decoHeight          = 35
                                      }
             }

-- | Don's preferred colors - from DynamicLog...;)
donaldTheme  :: ThemeInfo
donaldTheme =
    newTheme { themeName        = "donaldTheme"
             , themeAuthor      = "Andrea Rossato"
             , themeDescription = "Don's preferred colors - from DynamicLog...;)"
             , theme            = def { activeColor         = "#2b4f98"
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
             , theme            = def { activeColor         = "#aecf96"
                                      , inactiveColor       = "#111111"
                                      , activeBorderColor   = "#aecf96"
                                      , inactiveBorderColor = "#111111"
                                      , activeTextColor     = "black"
                                      , inactiveTextColor   = "#d5d3a7"
                                      , fontName            = "-*-profont-*-*-*-*-11-*-*-*-*-*-iso8859"
                                      , decoHeight          = 16
                                      }
             }

-- | Dark Theme, by Lucian Poston.
darkTheme :: ThemeInfo
darkTheme =
    newTheme { themeName        = "darkTheme"
             , themeAuthor      = "Lucian Poston"
             , themeDescription = "Dark Theme"
             , theme            = def { inactiveBorderColor = "#202030"
                                      , activeBorderColor   = "#a0a0d0"
                                      , inactiveColor       = "#000000"
                                      , activeColor         = "#000000"
                                      , inactiveTextColor   = "#607070"
                                      , activeTextColor     = "#a0d0d0"
                                      , decoHeight          = 15
                                      }
             }

-- | deifl\'s Theme, by deifl.
deiflTheme :: ThemeInfo
deiflTheme =
    newTheme { themeName        = "deiflTheme"
             , themeAuthor      = "deifl"
             , themeDescription = "deifl's Theme"
             , theme            = def { inactiveBorderColor = "#708090"
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
             , theme            = def { inactiveBorderColor = "#000"
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
             , theme            = def { activeColor         = "#4c7899"
                                      , inactiveColor       = "#333333"
                                      , activeBorderColor   = "#285577"
                                      , inactiveBorderColor = "#222222"
                                      , activeTextColor     = "#ffffff"
                                      , inactiveTextColor   = "#888888"
                                      , fontName            = "-*-fixed-medium-r-*--10-*-*-*-*-*-iso8859-1"
                                      , decoHeight          = 12
                                      }
             }

-- | Forest colours, by Kathryn Andersen
kavonForestTheme :: ThemeInfo
kavonForestTheme =
    newTheme { themeName        = "kavonForestTheme"
             , themeAuthor      = "Kathryn Andersen"
             , themeDescription = "Forest colours"
             , theme            = def { activeColor         = "#115422"
                                      , activeBorderColor   = "#1a8033"
                                      , activeTextColor     = "white"
                                      , inactiveColor       = "#543211"
                                      , inactiveBorderColor = "#804c19"
                                      , inactiveTextColor   = "#ffcc33"
                                      }
             }

-- | Lake (blue/green) colours, by Kathryn Andersen
kavonLakeTheme :: ThemeInfo
kavonLakeTheme =
    newTheme { themeName        = "kavonLakeTheme"
             , themeAuthor      = "Kathryn Andersen"
             , themeDescription = "Lake (blue/green) colours"
             , theme            = def { activeColor         = "#001166"
                                      , activeBorderColor   = "#1f3999"
                                      , activeTextColor     = "white"
                                      , inactiveColor       = "#09592a"
                                      , inactiveBorderColor = "#198044"
                                      , inactiveTextColor   = "#73e6a3"
                                      }
             }

-- | Peacock colours, by Kathryn Andersen
kavonPeacockTheme :: ThemeInfo
kavonPeacockTheme =
    newTheme { themeName        = "kavonPeacockTheme"
             , themeAuthor      = "Kathryn Andersen"
             , themeDescription = "Peacock colours"
             , theme            = def { activeColor         = "#190f4c"
                                      , activeBorderColor   = "#2b1980"
                                      , activeTextColor     = "white"
                                      , inactiveColor       = "#225173"
                                      , inactiveBorderColor = "#2a638c"
                                      , inactiveTextColor   = "#8fb2cc"
                                      }
             }

-- | Violet-Green colours, by Kathryn Andersen
kavonVioGreenTheme :: ThemeInfo
kavonVioGreenTheme =
    newTheme { themeName        = "kavonVioGreenTheme"
             , themeAuthor      = "Kathryn Andersen"
             , themeDescription = "Violet-Green colours"
             , theme            = def { activeColor         = "#37174c"
                                      , activeBorderColor   = "#333399"
                                      , activeTextColor     = "white"
                                      , inactiveColor       = "#174c17"
                                      , inactiveBorderColor = "#336633"
                                      , inactiveTextColor   = "#aaccaa"
                                      }
             }

-- | Blue colours, by Kathryn Andersen
kavonBluesTheme :: ThemeInfo
kavonBluesTheme =
    newTheme { themeName        = "kavonBluesTheme"
             , themeAuthor      = "Kathryn Andersen"
             , themeDescription = "Blue colours"
             , theme            = def { activeColor         = "#000066"
                                      , activeBorderColor   = "#111199"
                                      , activeTextColor     = "white"
                                      , inactiveColor       = "#9999ee"
                                      , inactiveBorderColor = "#6666cc"
                                      , inactiveTextColor   = "black"
                                      }
             }

-- | Christmas colours, by Kathryn Andersen
kavonChristmasTheme :: ThemeInfo
kavonChristmasTheme =
    newTheme { themeName        = "kavonChristmasTheme"
             , themeAuthor      = "Kathryn Andersen"
             , themeDescription = "Christmas (green + red) colours"
             , theme            = def { activeColor         = "#660000"
                                      , activeBorderColor   = "#990000"
                                      , activeTextColor     = "white"
                                      , inactiveColor       = "#006600"
                                      , inactiveBorderColor = "#003300"
                                      , inactiveTextColor   = "#99bb99"
                                      }
             }

-- | Autumn colours, by Kathryn Andersen
kavonAutumnTheme :: ThemeInfo
kavonAutumnTheme =
    newTheme { themeName        = "kavonAutumnTheme"
             , themeAuthor      = "Kathryn Andersen"
             , themeDescription = "Autumn (brown + red) colours"
             , theme            = def { activeColor         = "#660000"
                                      , activeBorderColor   = "#990000"
                                      , activeTextColor     = "white"
                                      , inactiveColor       = "#542d11"
                                      , inactiveBorderColor = "#804d1A"
                                      , inactiveTextColor   = "#ffcc33"
                                      }
             }

-- | Fire colours, by Kathryn Andersen
kavonFireTheme :: ThemeInfo
kavonFireTheme =
    newTheme { themeName        = "kavonFireTheme"
             , themeAuthor      = "Kathryn Andersen"
             , themeDescription = "Fire (orange + red) colours"
             , theme            = def { activeColor         = "#660000"
                                      , activeBorderColor   = "#990000"
                                      , activeTextColor     = "white"
                                      , inactiveColor       = "#ff8000"
                                      , inactiveBorderColor = "#d9b162"
                                      , inactiveTextColor   = "black"
                                      }
             }
