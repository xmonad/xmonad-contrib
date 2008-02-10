{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.DecorationMadness
-- Copyright   :  (c) 2007 Andrea Rossato
-- License     :  BSD-style (see xmonad/LICENSE)
--
-- Maintainer  :  andrea.rossato@unibz.it
-- Stability   :  unstable
-- Portability :  unportable
--
-- A collection of decorated layouts: some of them may be nice, some
-- usable, others just funny.
-----------------------------------------------------------------------------

module XMonad.Layout.DecorationMadness
    ( -- * Usage
      -- $usage

      -- * Decorated layouts based on Circle
      -- $circle
      circleSimpleDefault
    , circleDefault
    , circleSimpleDefaultResizable
    , circleDefaultResizable
    , circleSimpleDeco
    , circleSimpleDecoResizable
    , circleDeco
    , circleDecoResizable
    , circleSimpleDwmStyle
    , circleDwmStyle
    , circleSimpleTabbed
    , circleTabbed
    -- * Decorated layouts based on Accordion
    -- $accordion
    , accordionSimpleDefault
    , accordionDefault
    , accordionSimpleDefaultResizable
    , accordionDefaultResizable
    , accordionSimpleDeco
    , accordionSimpleDecoResizable
    , accordionDeco
    , accordionDecoResizable
    , accordionSimpleDwmStyle
    , accordionDwmStyle
    , accordionSimpleTabbed
    , accordionTabbed
    -- * Tall decorated layouts
    -- $tall
    , tallSimpleDefault
    , tallDefault
    , tallSimpleDefaultResizable
    , tallDefaultResizable
    , tallSimpleDeco
    , tallDeco
    , tallSimpleDecoResizable
    , tallDecoResizable
    , tallSimpleDwmStyle
    , tallDwmStyle
    , tallSimpleTabbed
    , tallTabbed
    -- * Mirror Tall decorated layouts
    -- $mirror
    , mirrorTallSimpleDefault
    , mirrorTallDefault
    , mirrorTallSimpleDefaultResizable
    , mirrorTallDefaultResizable
    , mirrorTallSimpleDeco
    , mirrorTallDeco
    , mirrorTallSimpleDecoResizable
    , mirrorTallDecoResizable
    , mirrorTallSimpleDwmStyle
    , mirrorTallDwmStyle
    , mirrorTallSimpleTabbed
    , mirrorTallTabbed
    -- * Floating decorated layouts
    -- $float
    , floatSimpleSimple
    , floatSimple
    , floatSimpleDefault
    , floatDefault
    , floatSimpleDwmStyle
    , floatDwmStyle
    , floatSimpleTabbed
    , floatTabbed
    , defaultTheme, shrinkText
    ) where

import Data.List
import XMonad
import qualified XMonad.StackSet as S
import XMonad.Layout.Decoration
import XMonad.Layout.DwmStyle
import XMonad.Layout.SimpleDecoration

import XMonad.Layout.Accordion
import XMonad.Layout.Circle
import XMonad.Layout.ResizeScreen
import XMonad.Layout.WindowArranger
import XMonad.Layout.SimpleFloat

-- $usage
-- You can use this module with the following in your
-- @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Layout.DecorationMadness
--
-- Then edit your @layoutHook@ by adding the layout you want:
--
-- > main = xmonad defaultConfig { layoutHook = someMadLayout }
--
-- For more detailed instructions on editing the layoutHook see:
--
-- "XMonad.Doc.Extending#Editing_the_layout_hook"
--
-- You can also edit the default theme:
--
-- > myTheme = defaultTheme { inactiveBorderColor = "#FF0000"
-- >                        , activeTextColor     = "#00FF00" }
--
-- and
--
-- > mylayout = tabbed shrinkText myTheme ||| Full ||| etc..
--
-- When a layout is resizable, this means two different things: you
-- can grab a window's decoration with the pointer and move it around,
-- and you can move and resize windows with the keyboard. For setting
-- up the key bindings, please read the documentation of
-- "XMonad.Layout.WindowArranger"
--
-- The deafult theme can be dynamically change with the xmonad theme
-- selector. See "XMonad.Prompt.Theme". For more themse, look at
-- "XMonad.Util.Themes"
--
-- NOTE: some of these layouts may not be working correctly with
-- WindowNavigation and with some layout combinators. I hope to fix
-- this problem shortly!

-- There may be a regression in Tabbed, and no tab is displayed when
-- using it with other layouts. This is the reason for the following
-- instance (to be removed!)
data SimpleTabbedDecoration a = SimpleTabbed deriving (Read, Show)
instance Eq a => DecorationStyle SimpleTabbedDecoration a where
    describeDeco  _ = "Tabbed"
    decorateFirst _ = True
    shrink    _ _ r = r
    pureDecoration _ _ ht (Rectangle x y wh _) s wrs (w,_) = Just $ Rectangle nx y nwh (fi ht)
        where nwh = wh `div` max 1 (fi $ length wrs)
              nx  = case w `elemIndex` (S.integrate s) of
                      Just i  -> x + (fi nwh * fi i)
                      Nothing -> x

-- $circle
-- Here you will find 'Circle' based decorated layouts.

-- | A 'Circle' layout with the xmonad default decoration, default
-- theme and default shrinker.
--
-- Here you can find a screen shot:
--
-- <http://code.haskell.org/~arossato/xmonadShots/circleSimpleDefault.png>
circleSimpleDefault :: ModifiedLayout (Decoration DefaultDecoration DefaultShrinker) Circle Window
circleSimpleDefault = decoration shrinkText defaultTheme DefaultDecoration Circle

-- | Similar to 'circleSimpleDefault' but with the possibility of
-- setting a custom shrinker and a custom theme.
circleDefault :: Shrinker s => s -> Theme
              -> ModifiedLayout (Decoration DefaultDecoration s) Circle Window
circleDefault s t = decoration s t DefaultDecoration Circle

-- | A 'Circle' layout with the xmonad simple decoration, default
-- theme and default shrinker.
--
-- Here you can find a screen shot:
--
-- <http://code.haskell.org/~arossato/xmonadShots/circleSimpleDeco.png>
circleSimpleDeco :: ModifiedLayout (Decoration SimpleDecoration DefaultShrinker) Circle Window
circleSimpleDeco = decoration shrinkText defaultTheme (Simple True) Circle

-- | Similar to 'circleSimpleDece' but with the possibility of
-- setting a custom shrinker and a custom theme.
circleDeco :: Shrinker s => s -> Theme
           -> ModifiedLayout (Decoration SimpleDecoration s) Circle Window
circleDeco s t = decoration s t (Simple True) Circle

-- | A 'Circle' layout with the xmonad default decoration, default
-- theme and default shrinker, but with the possibility of moving
-- windows with the mouse, and resize\/move them with the keyboard.
--
-- Here you can find a screen shot:
--
-- <http://code.haskell.org/~arossato/xmonadShots/circleSimpleDefaultResizable.png>
circleSimpleDefaultResizable :: ModifiedLayout (Decoration DefaultDecoration DefaultShrinker) (ModifiedLayout WindowArranger Circle) Window
circleSimpleDefaultResizable = decoration shrinkText defaultTheme DefaultDecoration (windowArrange Circle)

-- | Similar to 'circleSimpleDefaultResizable' but with the
-- possibility of setting a custom shrinker and a custom theme.
circleDefaultResizable :: Shrinker s => s -> Theme
                       -> ModifiedLayout (Decoration DefaultDecoration s) (ModifiedLayout WindowArranger Circle) Window
circleDefaultResizable s t = decoration s t DefaultDecoration (windowArrange Circle)

-- | A 'Circle' layout with the xmonad simple decoration, default
-- theme and default shrinker, but with the possibility of moving
-- windows with the mouse, and resize\/move them with the keyboard.
--
-- Here you can find a screen shot:
--
-- <http://code.haskell.org/~arossato/xmonadShots/circleSimpleDecoResizable.png>
circleSimpleDecoResizable :: ModifiedLayout (Decoration SimpleDecoration DefaultShrinker) (ModifiedLayout WindowArranger Circle) Window
circleSimpleDecoResizable = decoration shrinkText defaultTheme (Simple True) (windowArrange Circle)

-- | Similar to 'circleSimpleDecoResizable' but with the
-- possibility of setting a custom shrinker and a custom theme.
circleDecoResizable :: Shrinker s => s -> Theme
                    -> ModifiedLayout (Decoration SimpleDecoration s) (ModifiedLayout WindowArranger Circle) Window
circleDecoResizable s t = decoration s t (Simple True) (windowArrange Circle)

-- | A 'Circle' layout with the xmonad DwmStyle decoration, default
-- theme and default shrinker.
--
-- Here you can find a screen shot:
--
-- <http://code.haskell.org/~arossato/xmonadShots/circleSimpleDwmStyle.png>
circleSimpleDwmStyle :: ModifiedLayout (Decoration DwmStyle DefaultShrinker) Circle Window
circleSimpleDwmStyle = decoration shrinkText defaultTheme Dwm Circle

-- | Similar to 'circleSimpleDwmStyle' but with the
-- possibility of setting a custom shrinker and a custom theme.
circleDwmStyle :: Shrinker s => s -> Theme
               -> ModifiedLayout (Decoration DwmStyle s) Circle Window
circleDwmStyle s t = decoration s t Dwm Circle

-- | A 'Circle' layout with the xmonad tabbed decoration, default
-- theme and default shrinker.
--
-- Here you can find a screen shot:
--
-- <http://code.haskell.org/~arossato/xmonadShots/circleSimpleTabbed.png>
circleSimpleTabbed :: ModifiedLayout (Decoration SimpleTabbedDecoration DefaultShrinker) (ModifiedLayout ResizeScreen Circle) Window
circleSimpleTabbed = decoration shrinkText defaultTheme SimpleTabbed (resizeVertical 20 Circle)

-- | Similar to 'circleSimpleTabbed' but with the
-- possibility of setting a custom shrinker and a custom theme.
circleTabbed :: Shrinker s => s -> Theme
             -> ModifiedLayout (Decoration SimpleTabbedDecoration s) (ModifiedLayout ResizeScreen Circle) Window
circleTabbed s t = decoration s t SimpleTabbed (resizeVertical (fi $ decoHeight t) Circle)


-- $accordion
-- Here you will find decorated layouts based on the 'Accordion'
-- layout.

-- | An 'Accordion' layout with the xmonad default decoration, default
-- theme and default shrinker.
--
-- Here you can find a screen shot:
--
-- <http://code.haskell.org/~arossato/xmonadShots/accordionSimpleDefault.png>
accordionSimpleDefault :: ModifiedLayout (Decoration DefaultDecoration DefaultShrinker) Accordion Window
accordionSimpleDefault = decoration shrinkText defaultTheme DefaultDecoration Accordion

-- | Similar to 'accordionSimpleDefault' but with the possibility of
-- setting a custom shrinker and a custom theme.
accordionDefault :: Shrinker s => s -> Theme
                 -> ModifiedLayout (Decoration DefaultDecoration s) Accordion Window
accordionDefault s t = decoration s t DefaultDecoration Accordion

-- | An 'Accordion' layout with the xmonad simple decoration, default
-- theme and default shrinker.
--
-- Here you can find a screen shot:
--
-- <http://code.haskell.org/~arossato/xmonadShots/accordionSimpleDeco.png>
accordionSimpleDeco :: ModifiedLayout (Decoration SimpleDecoration DefaultShrinker) Accordion Window
accordionSimpleDeco = decoration shrinkText defaultTheme (Simple True) Accordion

-- | Similar to 'accordionSimpleDece' but with the possibility of
-- setting a custom shrinker and a custom theme.
accordionDeco :: Shrinker s => s -> Theme
              -> ModifiedLayout (Decoration SimpleDecoration s) Accordion Window
accordionDeco s t = decoration s t (Simple True) Accordion

-- | An 'Accordion' layout with the xmonad default decoration, default
-- theme and default shrinker, but with the possibility of moving
-- windows with the mouse, and resize\/move them with the keyboard.
accordionSimpleDefaultResizable :: ModifiedLayout (Decoration DefaultDecoration DefaultShrinker) (ModifiedLayout WindowArranger Accordion) Window
accordionSimpleDefaultResizable = decoration shrinkText defaultTheme DefaultDecoration (windowArrange Accordion)

-- | Similar to 'accordionSimpleDefaultResizable' but with the
-- possibility of setting a custom shrinker and a custom theme.
accordionDefaultResizable :: Shrinker s => s -> Theme
                          -> ModifiedLayout (Decoration DefaultDecoration s) (ModifiedLayout WindowArranger Accordion) Window
accordionDefaultResizable s t = decoration s t DefaultDecoration (windowArrange Accordion)

-- | An 'Accordion' layout with the xmonad simple decoration, default
-- theme and default shrinker, but with the possibility of moving
-- windows with the mouse, and resize\/move them with the keyboard.
accordionSimpleDecoResizable :: ModifiedLayout (Decoration SimpleDecoration DefaultShrinker) (ModifiedLayout WindowArranger Accordion) Window
accordionSimpleDecoResizable = decoration shrinkText defaultTheme (Simple True) (windowArrange Accordion)

-- | Similar to 'accordionSimpleDecoResizable' but with the
-- possibility of setting a custom shrinker and a custom theme.
accordionDecoResizable :: Shrinker s => s -> Theme
                       -> ModifiedLayout (Decoration SimpleDecoration s) (ModifiedLayout WindowArranger Accordion) Window
accordionDecoResizable s t = decoration s t (Simple True) (windowArrange Accordion)

-- | An 'Accordion' layout with the xmonad DwmStyle decoration, default
-- theme and default shrinker.
--
-- Here you can find a screen shot:
--
-- <http://code.haskell.org/~arossato/xmonadShots/accordionSimpleDwmStyle.png>
accordionSimpleDwmStyle :: ModifiedLayout (Decoration DwmStyle DefaultShrinker) Accordion Window
accordionSimpleDwmStyle = decoration shrinkText defaultTheme Dwm Accordion

-- | Similar to 'accordionSimpleDwmStyle' but with the
-- possibility of setting a custom shrinker and a custom theme.
accordionDwmStyle :: Shrinker s => s -> Theme
                  -> ModifiedLayout (Decoration DwmStyle s) Accordion Window
accordionDwmStyle s t = decoration s t Dwm Accordion

-- | An 'Accordion' layout with the xmonad tabbed decoration, default
-- theme and default shrinker.
--
-- Here you can find a screen shot:
--
-- <http://code.haskell.org/~arossato/xmonadShots/accordionSimpleTabbed.png>
accordionSimpleTabbed :: ModifiedLayout (Decoration SimpleTabbedDecoration DefaultShrinker) (ModifiedLayout ResizeScreen Accordion) Window
accordionSimpleTabbed = decoration shrinkText defaultTheme SimpleTabbed (resizeVertical 20 Accordion)

-- | Similar to 'accordionSimpleTabbed' but with the
-- possibility of setting a custom shrinker and a custom theme.
accordionTabbed :: Shrinker s => s -> Theme
                -> ModifiedLayout (Decoration SimpleTabbedDecoration s) (ModifiedLayout ResizeScreen Accordion) Window
accordionTabbed s t = decoration s t SimpleTabbed (resizeVertical (fi $ decoHeight t) Accordion)


-- $tall
-- In this section you will find decorated layouts based on the
-- 'Tall' layout.

tall :: Tall Window
tall = Tall 1 (3/100) (1/2)

-- | A 'Tall' layout with the xmonad default decoration, default
-- theme and default shrinker.
--
-- Here you can find a screen shot:
--
-- <http://code.haskell.org/~arossato/xmonadShots/tallSimpleDefault.png>
tallSimpleDefault :: ModifiedLayout (Decoration DefaultDecoration DefaultShrinker) Tall Window
tallSimpleDefault = decoration shrinkText defaultTheme DefaultDecoration tall

-- | Similar to 'tallSimpleDefault' but with the possibility of
-- setting a custom shrinker and a custom theme.
tallDefault :: Shrinker s => s -> Theme
            -> ModifiedLayout (Decoration DefaultDecoration s) Tall Window
tallDefault s t = decoration s t DefaultDecoration tall

-- | A 'Tall' layout with the xmonad simple decoration, default
-- theme and default shrinker.
--
-- Here you can find a screen shot:
--
-- <http://code.haskell.org/~arossato/xmonadShots/tallSimpleDeco.png>
tallSimpleDeco :: ModifiedLayout (Decoration SimpleDecoration DefaultShrinker) Tall Window
tallSimpleDeco = decoration shrinkText defaultTheme (Simple True) tall

-- | Similar to 'tallSimpleDece' but with the possibility of
-- setting a custom shrinker and a custom theme.
tallDeco :: Shrinker s => s -> Theme
         -> ModifiedLayout (Decoration SimpleDecoration s) Tall Window
tallDeco s t = decoration s t (Simple True) tall

-- | A 'Tall' layout with the xmonad default decoration, default
-- theme and default shrinker, but with the possibility of moving
-- windows with the mouse, and resize\/move them with the keyboard.
--
-- Here you can find a screen shot:
--
-- <http://code.haskell.org/~arossato/xmonadShots/tallSimpleDefaultResizable.png>
tallSimpleDefaultResizable :: ModifiedLayout (Decoration DefaultDecoration DefaultShrinker) (ModifiedLayout WindowArranger Tall) Window
tallSimpleDefaultResizable = decoration shrinkText defaultTheme DefaultDecoration (windowArrange tall)

-- | Similar to 'tallSimpleDefaultResizable' but with the
-- possibility of setting a custom shrinker and a custom theme.
tallDefaultResizable :: Shrinker s => s -> Theme
                     -> ModifiedLayout (Decoration DefaultDecoration s) (ModifiedLayout WindowArranger Tall) Window
tallDefaultResizable s t = decoration s t DefaultDecoration (windowArrange tall)

-- | A 'Tall' layout with the xmonad simple decoration, default
-- theme and default shrinker, but with the possibility of moving
-- windows with the mouse, and resize\/move them with the keyboard.
--
-- Here you can find a screen shot:
--
-- <http://code.haskell.org/~arossato/xmonadShots/tallSimpleDecoResizable.png>
tallSimpleDecoResizable :: ModifiedLayout (Decoration SimpleDecoration DefaultShrinker) (ModifiedLayout WindowArranger Tall) Window
tallSimpleDecoResizable = decoration shrinkText defaultTheme (Simple True) (windowArrange tall)

-- | Similar to 'tallSimpleDecoResizable' but with the
-- possibility of setting a custom shrinker and a custom theme.
tallDecoResizable :: Shrinker s => s -> Theme
                  -> ModifiedLayout (Decoration SimpleDecoration s) (ModifiedLayout WindowArranger Tall) Window
tallDecoResizable s t = decoration s t (Simple True) (windowArrange tall)

-- | A 'Tall' layout with the xmonad DwmStyle decoration, default
-- theme and default shrinker.
--
-- Here you can find a screen shot:
--
-- <http://code.haskell.org/~arossato/xmonadShots/tallSimpleDwmStyle.png>
tallSimpleDwmStyle :: ModifiedLayout (Decoration DwmStyle DefaultShrinker) Tall Window
tallSimpleDwmStyle = decoration shrinkText defaultTheme Dwm tall

-- | Similar to 'tallSimpleDwmStyle' but with the
-- possibility of setting a custom shrinker and a custom theme.
tallDwmStyle :: Shrinker s => s -> Theme
             -> ModifiedLayout (Decoration DwmStyle s) Tall Window
tallDwmStyle s t = decoration s t Dwm tall

-- | A 'Tall' layout with the xmonad tabbed decoration, default
-- theme and default shrinker.
--
-- Here you can find a screen shot:
--
-- <http://code.haskell.org/~arossato/xmonadShots/tallSimpleTabbed.png>
tallSimpleTabbed :: ModifiedLayout (Decoration SimpleTabbedDecoration DefaultShrinker) (ModifiedLayout ResizeScreen Tall) Window
tallSimpleTabbed = decoration shrinkText defaultTheme SimpleTabbed (resizeVertical 20 tall)

-- | Similar to 'tallSimpleTabbed' but with the
-- possibility of setting a custom shrinker and a custom theme.
tallTabbed :: Shrinker s => s -> Theme
           -> ModifiedLayout (Decoration SimpleTabbedDecoration s) (ModifiedLayout ResizeScreen Tall) Window
tallTabbed s t = decoration s t SimpleTabbed (resizeVertical (fi $ decoHeight t) tall)

-- $mirror
-- In this section you will find decorated layouts based on the
-- 'Mirror' layout modifier applied to 'Tall'.

mirrorTall :: Mirror Tall Window
mirrorTall = Mirror tall

-- | A 'Mirror Tall' layout with the xmonad default decoration, default
-- theme and default shrinker.
--
-- Here you can find a screen shot:
--
-- <http://code.haskell.org/~arossato/xmonadShots/mirrorTallSimpleDefault.png>
mirrorTallSimpleDefault :: ModifiedLayout (Decoration DefaultDecoration DefaultShrinker) (Mirror Tall) Window
mirrorTallSimpleDefault = decoration shrinkText defaultTheme DefaultDecoration mirrorTall

-- | Similar to 'mirrorTallSimpleDefault' but with the possibility of
-- setting a custom shrinker and a custom theme.
mirrorTallDefault :: Shrinker s => s -> Theme
                  -> ModifiedLayout (Decoration DefaultDecoration s) (Mirror Tall) Window
mirrorTallDefault s t = decoration s t DefaultDecoration mirrorTall

-- | A 'Mirror Tall' layout with the xmonad simple decoration, default
-- theme and default shrinker.
--
-- Here you can find a screen shot:
--
-- <http://code.haskell.org/~arossato/xmonadShots/mirrorTallSimpleDeco.png>
mirrorTallSimpleDeco :: ModifiedLayout (Decoration SimpleDecoration DefaultShrinker) (Mirror Tall) Window
mirrorTallSimpleDeco = decoration shrinkText defaultTheme (Simple True) mirrorTall

-- | Similar to 'mirrorTallSimpleDece' but with the possibility of
-- setting a custom shrinker and a custom theme.
mirrorTallDeco :: Shrinker s => s -> Theme
               -> ModifiedLayout (Decoration SimpleDecoration s) (Mirror Tall) Window
mirrorTallDeco s t = decoration s t (Simple True) mirrorTall

-- | A 'Mirror Tall' layout with the xmonad default decoration, default
-- theme and default shrinker, but with the possibility of moving
-- windows with the mouse, and resize\/move them with the keyboard.
--
-- Here you can find a screen shot:
--
-- <http://code.haskell.org/~arossato/xmonadShots/mirrorTallSimpleDefaultResizable.png>
mirrorTallSimpleDefaultResizable :: ModifiedLayout (Decoration DefaultDecoration DefaultShrinker) (ModifiedLayout WindowArranger (Mirror Tall)) Window
mirrorTallSimpleDefaultResizable = decoration shrinkText defaultTheme DefaultDecoration (windowArrange mirrorTall)

-- | Similar to 'mirrorTallSimpleDefaultResizable' but with the
-- possibility of setting a custom shrinker and a custom theme.
mirrorTallDefaultResizable :: Shrinker s => s -> Theme
                           -> ModifiedLayout (Decoration DefaultDecoration s) (ModifiedLayout WindowArranger (Mirror Tall)) Window
mirrorTallDefaultResizable s t = decoration s t DefaultDecoration (windowArrange mirrorTall)

-- | A 'Mirror Tall' layout with the xmonad simple decoration, default
-- theme and default shrinker, but with the possibility of moving
-- windows with the mouse, and resize\/move them with the keyboard.
--
-- Here you can find a screen shot:
--
-- <http://code.haskell.org/~arossato/xmonadShots/mirrorTallSimpleDecoResizable.png>
mirrorTallSimpleDecoResizable :: ModifiedLayout (Decoration SimpleDecoration DefaultShrinker) (ModifiedLayout WindowArranger (Mirror Tall)) Window
mirrorTallSimpleDecoResizable = decoration shrinkText defaultTheme (Simple True) (windowArrange mirrorTall)

-- | Similar to 'mirrorTallSimpleDecoResizable' but with the
-- possibility of setting a custom shrinker and a custom theme.
mirrorTallDecoResizable :: Shrinker s => s -> Theme
                        -> ModifiedLayout (Decoration SimpleDecoration s) (ModifiedLayout WindowArranger (Mirror Tall)) Window
mirrorTallDecoResizable s t = decoration s t (Simple True) (windowArrange mirrorTall)

-- | A 'Mirror Tall' layout with the xmonad DwmStyle decoration, default
-- theme and default shrinker.
--
-- Here you can find a screen shot:
--
-- <http://code.haskell.org/~arossato/xmonadShots/mirrorTallSimpleDwmStyle.png>
mirrorTallSimpleDwmStyle :: ModifiedLayout (Decoration DwmStyle DefaultShrinker) (Mirror Tall) Window
mirrorTallSimpleDwmStyle = decoration shrinkText defaultTheme Dwm mirrorTall

-- | Similar to 'mirrorTallSimpleDwmStyle' but with the
-- possibility of setting a custom shrinker and a custom theme.
mirrorTallDwmStyle :: Shrinker s => s -> Theme
                   -> ModifiedLayout (Decoration DwmStyle s) (Mirror Tall) Window
mirrorTallDwmStyle s t = decoration s t Dwm mirrorTall

-- | A 'Mirror Tall' layout with the xmonad tabbed decoration, default
-- theme and default shrinker.
--
-- Here you can find a screen shot:
--
-- <http://code.haskell.org/~arossato/xmonadShots/mirrorTallSimpleTabbed.png>
mirrorTallSimpleTabbed :: ModifiedLayout (Decoration SimpleTabbedDecoration DefaultShrinker) (ModifiedLayout ResizeScreen (Mirror Tall)) Window
mirrorTallSimpleTabbed = decoration shrinkText defaultTheme SimpleTabbed (resizeVertical 20 mirrorTall)

-- | Similar to 'mirrorTallSimpleTabbed' but with the
-- possibility of setting a custom shrinker and a custom theme.
mirrorTallTabbed :: Shrinker s => s -> Theme
                 -> ModifiedLayout (Decoration SimpleTabbedDecoration s) (ModifiedLayout ResizeScreen (Mirror Tall)) Window
mirrorTallTabbed s t = decoration s t SimpleTabbed (resizeVertical (fi $ decoHeight t) mirrorTall)

-- $float
-- Here you will find decorated layout based on the SimpleFloating
-- layout

-- | A simple floating layout where every window is placed according
-- to the window's initial attributes.
--
-- Here you can find a screen shot:
--
-- <http://code.haskell.org/~arossato/xmonadShots/floatSimpleSimple.png>
floatSimpleSimple :: ModifiedLayout (Decoration SimpleDecoration DefaultShrinker)
	             (ModifiedLayout WindowArranger SimpleFloat) a
floatSimpleSimple = simpleFloat

floatSimple :: Shrinker s => s -> Theme ->
               ModifiedLayout (Decoration SimpleDecoration s)
	      (ModifiedLayout WindowArranger SimpleFloat) a
floatSimple = simpleFloat'

-- | This version is decorated with the 'DefaultDecoration' style.
--
-- Here you can find a screen shot:
--
-- <http://code.haskell.org/~arossato/xmonadShots/floatSimpleDefault.png>
floatSimpleDefault :: ModifiedLayout (Decoration DefaultDecoration DefaultShrinker)
	              (ModifiedLayout WindowArranger SimpleFloat) a
floatSimpleDefault = decoration shrinkText defaultTheme DefaultDecoration (windowArrangeAll $ SF 20)

-- | Same as 'floatSimpleDefault', but with the possibility of setting a
-- custom shrinker and a custom theme.
floatDefault :: Shrinker s => s -> Theme ->
                ModifiedLayout (Decoration DefaultDecoration s)
	       (ModifiedLayout WindowArranger SimpleFloat) a
floatDefault s c = decoration s c DefaultDecoration (windowArrangeAll $ SF (decoHeight c))

-- | This version is decorated with the 'DwmStyle'. Note that this is
-- a keyboard only floating layout.
--
-- Here you can find a screen shot:
--
-- <http://code.haskell.org/~arossato/xmonadShots/floatSimpleDwmStyle.png>
floatSimpleDwmStyle :: Eq a => ModifiedLayout (Decoration DwmStyle DefaultShrinker)
	               (ModifiedLayout WindowArranger SimpleFloat) a
floatSimpleDwmStyle = decoration shrinkText defaultTheme Dwm (windowArrangeAll $ SF 20)

-- | Same as 'floatSimpleDwmStyle', but with the possibility of setting a
-- custom shrinker and a custom theme.
floatDwmStyle :: (Eq a, Shrinker s) => s -> Theme ->
                 ModifiedLayout (Decoration DwmStyle s)
	        (ModifiedLayout WindowArranger SimpleFloat) a
floatDwmStyle s c = decoration s c Dwm (windowArrangeAll $ SF (decoHeight c))

-- | This version is decorated with the 'TabbedDecoration' style.
-- | Mouse dragging is somehow weird.
--
-- Here you can find a screen shot:
--
-- <http://code.haskell.org/~arossato/xmonadShots/floatSimpleTabbed.png>
floatSimpleTabbed :: Eq a => ModifiedLayout (Decoration SimpleTabbedDecoration DefaultShrinker)
	       (ModifiedLayout WindowArranger SimpleFloat) a
floatSimpleTabbed = decoration shrinkText defaultTheme SimpleTabbed (windowArrangeAll $ SF 20)

-- | Same as 'floatSimpleTabbed', but with the possibility of setting a
-- custom shrinker and a custom theme.
floatTabbed :: (Eq a, Shrinker s) => s -> Theme ->
               ModifiedLayout (Decoration SimpleTabbedDecoration s)
	      (ModifiedLayout WindowArranger SimpleFloat) a
floatTabbed s c = decoration s c SimpleTabbed (windowArrangeAll $ SF (decoHeight c))
