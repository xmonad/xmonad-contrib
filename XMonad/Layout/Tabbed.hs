{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, PatternGuards, TypeSynonymInstances #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.Tabbed
-- Copyright   :  (c) 2007 David Roundy, Andrea Rossato
-- License     :  BSD-style (see xmonad/LICENSE)
--
-- Maintainer  :  droundy@darcs.net, andrea.rossato@unibz.it
-- Stability   :  unstable
-- Portability :  unportable
--
-- A tabbed layout for the Xmonad Window Manager
--
-- This module has functions and types that conflict with those used
-- in Decoration.hs. These functions and types are deprecated and will
-- be removed.
--
-- PLEASE: do not use 'tabbed'. Use 'tabDeco' instead.
--
-----------------------------------------------------------------------------

module XMonad.Layout.Tabbed
    ( -- * Usage:
      -- $usage
      tabbed
    , tabDeco
    , TConf (..), defaultTConf
    , TabbedDecoration (..), defaultTabbedConfig
    , shrinkText, CustomShrink(CustomShrink)
    , Shrinker(..)
    ) where

import Data.Maybe
import Data.List

import XMonad
import qualified XMonad.StackSet as S
import XMonad.Layout.Decoration
import XMonad.Layout.Simplest

-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Layout.Tabbed
--
-- Then edit your @layoutHook@ by adding the Tabbed layout:
--
-- > myLayouts = tabDeco shrinkText defaultTabbedConfig ||| Full ||| etc..
-- > main = xmonad defaultConfig { layoutHook = myLayouts }
--
-- For more detailed instructions on editing the layoutHook see:
--
-- "XMonad.Doc.Extending#Editing_the_layout_hook"
--
-- You can also edit the default configuration options.
--
-- > myTabConfig = defaultTabbedConfig { inactiveBorderColor = "#FF0000"
-- >                                   , activeTextColor = "#00FF00"}
--
-- and
--
-- > mylayout = tabDeco shrinkText myTabConfig ||| Full ||| etc..

-- | Create a tabbed layout with a shrinker and a tabbed configuration.
tabDeco :: (Eq a, Shrinker s) => s -> DeConfig TabbedDecoration a
        -> ModifiedLayout (Decoration TabbedDecoration s) Simplest  a
tabDeco s c = decoration s c Simplest

-- | This function is deprecated and will be removed before 0.7!!
tabbed :: (Eq a, Shrinker s) => s -> TConf
       -> ModifiedLayout (Decoration TabbedDecoration s) Simplest a
tabbed s c = decoration s (toNewConf c) Simplest

defaultTabbedConfig :: Eq a => DeConfig TabbedDecoration a
defaultTabbedConfig = mkDefaultDeConfig $ Tabbed

data TabbedDecoration a = Tabbed deriving (Read, Show)

instance Eq a => DecorationStyle TabbedDecoration a where
    describeDeco  _ = "Tabbed"
    decorateFirst _ = False
    pureDecoration _ _ ht (Rectangle x y wh _) s wrs (w,_) = Just $ Rectangle nx y nwh (fi ht)
        where nwh = wh `div` max 1 (fi $ length wrs)
              nx  = case w `elemIndex` (S.integrate s) of
                      Just i  -> x + (fi nwh * fi i)
                      Nothing -> x

-- Backward compatibility stuff
-- DEPRECATED!!
toNewConf :: Eq a => TConf -> DeConfig TabbedDecoration a
toNewConf oc =
    nc { XMonad.Layout.Decoration.activeColor         = XMonad.Layout.Tabbed.activeColor         oc
       , XMonad.Layout.Decoration.inactiveColor       = XMonad.Layout.Tabbed.inactiveColor       oc
       , XMonad.Layout.Decoration.urgentColor         = XMonad.Layout.Tabbed.urgentColor         oc
       , XMonad.Layout.Decoration.activeBorderColor   = XMonad.Layout.Tabbed.activeBorderColor   oc
       , XMonad.Layout.Decoration.inactiveBorderColor = XMonad.Layout.Tabbed.inactiveBorderColor oc
       , XMonad.Layout.Decoration.urgentBorderColor   = XMonad.Layout.Tabbed.urgentBorderColor   oc
       , XMonad.Layout.Decoration.activeTextColor     = XMonad.Layout.Tabbed.activeTextColor     oc
       , XMonad.Layout.Decoration.inactiveTextColor   = XMonad.Layout.Tabbed.inactiveTextColor   oc
       , XMonad.Layout.Decoration.urgentTextColor     = XMonad.Layout.Tabbed.urgentTextColor     oc
       , XMonad.Layout.Decoration.fontName            = XMonad.Layout.Tabbed.fontName            oc
       , XMonad.Layout.Decoration.decoHeight     = fi $ XMonad.Layout.Tabbed.tabSize             oc
       }
    where nc = mkDefaultDeConfig $ Tabbed

-- | This datatype is deprecated and will be removed before 0.7!!
data TConf =
    TConf { activeColor         :: String
          , inactiveColor       :: String
          , urgentColor         :: String
          , activeBorderColor   :: String
          , inactiveBorderColor :: String
          , urgentBorderColor   :: String
          , activeTextColor     :: String
          , inactiveTextColor   :: String
          , urgentTextColor     :: String
          , fontName            :: String
          , tabSize             :: Int
          } deriving (Show, Read)

-- | This function is deprecated and will be removed before 0.7!!
defaultTConf :: TConf
defaultTConf =
    TConf { XMonad.Layout.Tabbed.activeColor         = "#999999"
          , XMonad.Layout.Tabbed.inactiveColor       = "#666666"
          , XMonad.Layout.Tabbed.urgentColor         = "#FFFF00"
          , XMonad.Layout.Tabbed.activeBorderColor   = "#FFFFFF"
          , XMonad.Layout.Tabbed.inactiveBorderColor = "#BBBBBB"
          , XMonad.Layout.Tabbed.urgentBorderColor   = "##00FF00"
          , XMonad.Layout.Tabbed.activeTextColor     = "#FFFFFF"
          , XMonad.Layout.Tabbed.inactiveTextColor   = "#BFBFBF"
          , XMonad.Layout.Tabbed.urgentTextColor     = "#FF0000"
          , XMonad.Layout.Tabbed.fontName            = "-misc-fixed-*-*-*-*-10-*-*-*-*-*-*-*"
          , XMonad.Layout.Tabbed.tabSize             = 20
          }

