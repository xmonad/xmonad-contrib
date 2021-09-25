-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.DecorationMadness
-- Description :  A collection of decorated layouts.
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
    , def, shrinkText
    ) where

import XMonad
import XMonad.Actions.MouseResize
import XMonad.Layout.Decoration
import XMonad.Layout.DwmStyle
import XMonad.Layout.SimpleDecoration
import XMonad.Layout.TabBarDecoration

import XMonad.Layout.Accordion
import XMonad.Layout.Circle
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
-- > main = xmonad def { layoutHook = someMadLayout }
--
-- For more detailed instructions on editing the layoutHook see:
--
-- "XMonad.Doc.Extending#Editing_the_layout_hook"
--
-- You can also edit the default theme:
--
-- > myTheme = def { inactiveBorderColor = "#FF0000"
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
-- The default theme can be dynamically change with the xmonad theme
-- selector. See "XMonad.Prompt.Theme". For more themes, look at
-- "XMonad.Util.Themes"

-- $circle
-- Here you will find 'Circle' based decorated layouts.

-- | A 'Circle' layout with the xmonad default decoration, default
-- theme and default shrinker.
--
-- Here you can find a screen shot:
--
-- <http://code.haskell.org/~arossato/xmonadShots/circleSimpleDefault.png>
circleSimpleDefault :: ModifiedLayout (Decoration DefaultDecoration DefaultShrinker) Circle Window
circleSimpleDefault = decoration shrinkText def DefaultDecoration Circle

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
circleSimpleDeco = decoration shrinkText def (Simple True) Circle

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
circleSimpleDefaultResizable :: ModifiedLayout (Decoration DefaultDecoration DefaultShrinker)
                                (ModifiedLayout MouseResize (ModifiedLayout WindowArranger Circle)) Window
circleSimpleDefaultResizable = decoration shrinkText def DefaultDecoration (mouseResize $ windowArrange Circle)

-- | Similar to 'circleSimpleDefaultResizable' but with the
-- possibility of setting a custom shrinker and a custom theme.
circleDefaultResizable :: Shrinker s => s -> Theme
                       -> ModifiedLayout (Decoration DefaultDecoration s)
                          (ModifiedLayout MouseResize (ModifiedLayout WindowArranger Circle)) Window
circleDefaultResizable s t = decoration s t DefaultDecoration (mouseResize $ windowArrange Circle)

-- | A 'Circle' layout with the xmonad simple decoration, default
-- theme and default shrinker, but with the possibility of moving
-- windows with the mouse, and resize\/move them with the keyboard.
--
-- Here you can find a screen shot:
--
-- <http://code.haskell.org/~arossato/xmonadShots/circleSimpleDecoResizable.png>
circleSimpleDecoResizable :: ModifiedLayout (Decoration SimpleDecoration DefaultShrinker)
                             (ModifiedLayout MouseResize (ModifiedLayout WindowArranger Circle)) Window
circleSimpleDecoResizable = decoration shrinkText def (Simple True) (mouseResize $ windowArrange Circle)

-- | Similar to 'circleSimpleDecoResizable' but with the
-- possibility of setting a custom shrinker and a custom theme.
circleDecoResizable :: Shrinker s => s -> Theme
                    -> ModifiedLayout (Decoration SimpleDecoration s)
                       (ModifiedLayout MouseResize (ModifiedLayout WindowArranger Circle)) Window
circleDecoResizable s t = decoration s t (Simple True) (mouseResize $ windowArrange Circle)

-- | A 'Circle' layout with the xmonad DwmStyle decoration, default
-- theme and default shrinker.
--
-- Here you can find a screen shot:
--
-- <http://code.haskell.org/~arossato/xmonadShots/circleSimpleDwmStyle.png>
circleSimpleDwmStyle :: ModifiedLayout (Decoration DwmStyle DefaultShrinker) Circle Window
circleSimpleDwmStyle = decoration shrinkText def Dwm Circle

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
circleSimpleTabbed :: ModifiedLayout (Decoration TabBarDecoration DefaultShrinker) (ModifiedLayout ResizeScreen Circle) Window
circleSimpleTabbed = simpleTabBar Circle

-- | Similar to 'circleSimpleTabbed' but with the
-- possibility of setting a custom shrinker and a custom theme.
circleTabbed :: Shrinker s => s -> Theme
             -> ModifiedLayout (Decoration TabBarDecoration s) (ModifiedLayout ResizeScreen Circle) Window
circleTabbed s t = tabBar s t Top (resizeVertical (fi $ decoHeight t) Circle)


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
accordionSimpleDefault = decoration shrinkText def DefaultDecoration Accordion

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
accordionSimpleDeco = decoration shrinkText def (Simple True) Accordion

-- | Similar to 'accordionSimpleDece' but with the possibility of
-- setting a custom shrinker and a custom theme.
accordionDeco :: Shrinker s => s -> Theme
              -> ModifiedLayout (Decoration SimpleDecoration s) Accordion Window
accordionDeco s t = decoration s t (Simple True) Accordion

-- | An 'Accordion' layout with the xmonad default decoration, default
-- theme and default shrinker, but with the possibility of moving
-- windows with the mouse, and resize\/move them with the keyboard.
accordionSimpleDefaultResizable :: ModifiedLayout (Decoration DefaultDecoration DefaultShrinker)
                                   (ModifiedLayout MouseResize (ModifiedLayout WindowArranger Accordion)) Window
accordionSimpleDefaultResizable = decoration shrinkText def DefaultDecoration (mouseResize $ windowArrange Accordion)

-- | Similar to 'accordionSimpleDefaultResizable' but with the
-- possibility of setting a custom shrinker and a custom theme.
accordionDefaultResizable :: Shrinker s => s -> Theme
                          -> ModifiedLayout (Decoration DefaultDecoration s)
                             (ModifiedLayout MouseResize (ModifiedLayout WindowArranger Accordion)) Window
accordionDefaultResizable s t = decoration s t DefaultDecoration (mouseResize $ windowArrange Accordion)

-- | An 'Accordion' layout with the xmonad simple decoration, default
-- theme and default shrinker, but with the possibility of moving
-- windows with the mouse, and resize\/move them with the keyboard.
accordionSimpleDecoResizable :: ModifiedLayout (Decoration SimpleDecoration DefaultShrinker)
                                (ModifiedLayout MouseResize (ModifiedLayout WindowArranger Accordion)) Window
accordionSimpleDecoResizable = decoration shrinkText def (Simple True) (mouseResize $ windowArrange Accordion)

-- | Similar to 'accordionSimpleDecoResizable' but with the
-- possibility of setting a custom shrinker and a custom theme.
accordionDecoResizable :: Shrinker s => s -> Theme
                       -> ModifiedLayout (Decoration SimpleDecoration s)
                          (ModifiedLayout MouseResize (ModifiedLayout WindowArranger Accordion)) Window
accordionDecoResizable s t = decoration s t (Simple True) (mouseResize $ windowArrange Accordion)

-- | An 'Accordion' layout with the xmonad DwmStyle decoration, default
-- theme and default shrinker.
--
-- Here you can find a screen shot:
--
-- <http://code.haskell.org/~arossato/xmonadShots/accordionSimpleDwmStyle.png>
accordionSimpleDwmStyle :: ModifiedLayout (Decoration DwmStyle DefaultShrinker) Accordion Window
accordionSimpleDwmStyle = decoration shrinkText def Dwm Accordion

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
accordionSimpleTabbed :: ModifiedLayout (Decoration TabBarDecoration DefaultShrinker) (ModifiedLayout ResizeScreen Accordion) Window
accordionSimpleTabbed = simpleTabBar Accordion

-- | Similar to 'accordionSimpleTabbed' but with the
-- possibility of setting a custom shrinker and a custom theme.
accordionTabbed :: Shrinker s => s -> Theme
                -> ModifiedLayout (Decoration TabBarDecoration s) (ModifiedLayout ResizeScreen Accordion) Window
accordionTabbed s t = tabBar s t Top (resizeVertical (fi $ decoHeight t) Accordion)


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
tallSimpleDefault = decoration shrinkText def DefaultDecoration tall

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
tallSimpleDeco = decoration shrinkText def (Simple True) tall

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
tallSimpleDefaultResizable :: ModifiedLayout (Decoration DefaultDecoration DefaultShrinker)
                              (ModifiedLayout MouseResize (ModifiedLayout WindowArranger Tall)) Window
tallSimpleDefaultResizable = decoration shrinkText def DefaultDecoration (mouseResize $ windowArrange tall)

-- | Similar to 'tallSimpleDefaultResizable' but with the
-- possibility of setting a custom shrinker and a custom theme.
tallDefaultResizable :: Shrinker s => s -> Theme
                     -> ModifiedLayout (Decoration DefaultDecoration s)
                        (ModifiedLayout MouseResize (ModifiedLayout WindowArranger Tall)) Window
tallDefaultResizable s t = decoration s t DefaultDecoration (mouseResize $ windowArrange tall)

-- | A 'Tall' layout with the xmonad simple decoration, default
-- theme and default shrinker, but with the possibility of moving
-- windows with the mouse, and resize\/move them with the keyboard.
--
-- Here you can find a screen shot:
--
-- <http://code.haskell.org/~arossato/xmonadShots/tallSimpleDecoResizable.png>
tallSimpleDecoResizable :: ModifiedLayout (Decoration SimpleDecoration DefaultShrinker)
                           (ModifiedLayout MouseResize (ModifiedLayout WindowArranger Tall)) Window
tallSimpleDecoResizable = decoration shrinkText def (Simple True) (mouseResize $ windowArrange tall)

-- | Similar to 'tallSimpleDecoResizable' but with the
-- possibility of setting a custom shrinker and a custom theme.
tallDecoResizable :: Shrinker s => s -> Theme
                  -> ModifiedLayout (Decoration SimpleDecoration s)
                     (ModifiedLayout MouseResize (ModifiedLayout WindowArranger Tall)) Window
tallDecoResizable s t = decoration s t (Simple True) (mouseResize $ windowArrange tall)

-- | A 'Tall' layout with the xmonad DwmStyle decoration, default
-- theme and default shrinker.
--
-- Here you can find a screen shot:
--
-- <http://code.haskell.org/~arossato/xmonadShots/tallSimpleDwmStyle.png>
tallSimpleDwmStyle :: ModifiedLayout (Decoration DwmStyle DefaultShrinker) Tall Window
tallSimpleDwmStyle = decoration shrinkText def Dwm tall

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
tallSimpleTabbed :: ModifiedLayout (Decoration TabBarDecoration DefaultShrinker) (ModifiedLayout ResizeScreen Tall) Window
tallSimpleTabbed = simpleTabBar tall

-- | Similar to 'tallSimpleTabbed' but with the
-- possibility of setting a custom shrinker and a custom theme.
tallTabbed :: Shrinker s => s -> Theme
           -> ModifiedLayout (Decoration TabBarDecoration s) (ModifiedLayout ResizeScreen Tall) Window
tallTabbed s t = tabBar s t Top (resizeVertical (fi $ decoHeight t) tall)

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
mirrorTallSimpleDefault = decoration shrinkText def DefaultDecoration mirrorTall

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
mirrorTallSimpleDeco = decoration shrinkText def (Simple True) mirrorTall

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
mirrorTallSimpleDefaultResizable :: ModifiedLayout (Decoration DefaultDecoration DefaultShrinker)
                                    (ModifiedLayout MouseResize (ModifiedLayout WindowArranger (Mirror Tall))) Window
mirrorTallSimpleDefaultResizable = decoration shrinkText def DefaultDecoration (mouseResize $ windowArrange mirrorTall)

-- | Similar to 'mirrorTallSimpleDefaultResizable' but with the
-- possibility of setting a custom shrinker and a custom theme.
mirrorTallDefaultResizable :: Shrinker s => s -> Theme
                           -> ModifiedLayout (Decoration DefaultDecoration s)
                              (ModifiedLayout MouseResize (ModifiedLayout WindowArranger (Mirror Tall))) Window
mirrorTallDefaultResizable s t = decoration s t DefaultDecoration (mouseResize $ windowArrange mirrorTall)

-- | A 'Mirror Tall' layout with the xmonad simple decoration, default
-- theme and default shrinker, but with the possibility of moving
-- windows with the mouse, and resize\/move them with the keyboard.
--
-- Here you can find a screen shot:
--
-- <http://code.haskell.org/~arossato/xmonadShots/mirrorTallSimpleDecoResizable.png>
mirrorTallSimpleDecoResizable :: ModifiedLayout (Decoration SimpleDecoration DefaultShrinker)
                                 (ModifiedLayout MouseResize (ModifiedLayout WindowArranger (Mirror Tall))) Window
mirrorTallSimpleDecoResizable = decoration shrinkText def (Simple True) (mouseResize $ windowArrange mirrorTall)

-- | Similar to 'mirrorTallSimpleDecoResizable' but with the
-- possibility of setting a custom shrinker and a custom theme.
mirrorTallDecoResizable :: Shrinker s => s -> Theme
                        -> ModifiedLayout (Decoration SimpleDecoration s)
                           (ModifiedLayout MouseResize (ModifiedLayout WindowArranger (Mirror Tall))) Window
mirrorTallDecoResizable s t = decoration s t (Simple True) (mouseResize $ windowArrange mirrorTall)

-- | A 'Mirror Tall' layout with the xmonad DwmStyle decoration, default
-- theme and default shrinker.
--
-- Here you can find a screen shot:
--
-- <http://code.haskell.org/~arossato/xmonadShots/mirrorTallSimpleDwmStyle.png>
mirrorTallSimpleDwmStyle :: ModifiedLayout (Decoration DwmStyle DefaultShrinker) (Mirror Tall) Window
mirrorTallSimpleDwmStyle = decoration shrinkText def Dwm mirrorTall

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
mirrorTallSimpleTabbed :: ModifiedLayout (Decoration TabBarDecoration DefaultShrinker) (ModifiedLayout ResizeScreen (Mirror Tall)) Window
mirrorTallSimpleTabbed = simpleTabBar mirrorTall

-- | Similar to 'mirrorTallSimpleTabbed' but with the
-- possibility of setting a custom shrinker and a custom theme.
mirrorTallTabbed :: Shrinker s => s -> Theme
                 -> ModifiedLayout (Decoration TabBarDecoration s) (ModifiedLayout ResizeScreen (Mirror Tall)) Window
mirrorTallTabbed s t = tabBar s t Top (resizeVertical (fi $ decoHeight t) mirrorTall)

-- $float
-- Here you will find decorated layout based on the SimpleFloating
-- layout

-- | A simple floating layout where every window is placed according
-- to the window's initial attributes.
--
-- Here you can find a screen shot:
--
-- <http://code.haskell.org/~arossato/xmonadShots/floatSimpleSimple.png>
floatSimpleSimple :: (Show a, Eq a) => ModifiedLayout (Decoration SimpleDecoration DefaultShrinker)
                 (ModifiedLayout MouseResize (ModifiedLayout WindowArranger SimpleFloat)) a
floatSimpleSimple = simpleFloat

floatSimple :: (Show a, Eq a, Shrinker s) => s -> Theme ->
               ModifiedLayout (Decoration SimpleDecoration s)
          (ModifiedLayout MouseResize (ModifiedLayout WindowArranger SimpleFloat)) a
floatSimple = simpleFloat'

-- | This version is decorated with the 'DefaultDecoration' style.
--
-- Here you can find a screen shot:
--
-- <http://code.haskell.org/~arossato/xmonadShots/floatSimpleDefault.png>
floatSimpleDefault :: (Show a, Eq a) => ModifiedLayout (Decoration DefaultDecoration DefaultShrinker)
                  (ModifiedLayout MouseResize (ModifiedLayout WindowArranger SimpleFloat)) a
floatSimpleDefault = decoration shrinkText def DefaultDecoration (mouseResize $ windowArrangeAll $ SF 20)

-- | Same as 'floatSimpleDefault', but with the possibility of setting a
-- custom shrinker and a custom theme.
floatDefault :: (Show a, Eq a, Shrinker s) => s -> Theme ->
                ModifiedLayout (Decoration DefaultDecoration s)
           (ModifiedLayout MouseResize (ModifiedLayout WindowArranger SimpleFloat)) a
floatDefault s t = decoration s t DefaultDecoration (mouseResize $ windowArrangeAll $ SF (decoHeight t))

-- | This version is decorated with the 'DwmStyle'. Note that this is
-- a keyboard only floating layout.
--
-- Here you can find a screen shot:
--
-- <http://code.haskell.org/~arossato/xmonadShots/floatSimpleDwmStyle.png>
floatSimpleDwmStyle :: (Show a, Eq a) => ModifiedLayout (Decoration DwmStyle DefaultShrinker)
                   (ModifiedLayout MouseResize (ModifiedLayout WindowArranger SimpleFloat)) a
floatSimpleDwmStyle = decoration shrinkText def Dwm (mouseResize $ windowArrangeAll $ SF 20)

-- | Same as 'floatSimpleDwmStyle', but with the possibility of setting a
-- custom shrinker and a custom theme.
floatDwmStyle :: (Show a, Eq a, Shrinker s) => s -> Theme ->
                 ModifiedLayout (Decoration DwmStyle s)
            (ModifiedLayout MouseResize (ModifiedLayout WindowArranger SimpleFloat)) a
floatDwmStyle s t = decoration s t Dwm (mouseResize $ windowArrangeAll $ SF (decoHeight t))

-- | This version is decorated with the 'TabbedDecoration' style.
-- | Mouse dragging is somehow weird.
--
-- Here you can find a screen shot:
--
-- <http://code.haskell.org/~arossato/xmonadShots/floatSimpleTabbed.png>
floatSimpleTabbed :: (Show a, Eq a) => ModifiedLayout (Decoration TabBarDecoration DefaultShrinker)
                 (ModifiedLayout MouseResize (ModifiedLayout WindowArranger SimpleFloat)) a
floatSimpleTabbed = tabBar shrinkText def Top (mouseResize $ windowArrangeAll $ SF 20)

-- | Same as 'floatSimpleTabbed', but with the possibility of setting a
-- custom shrinker and a custom theme.
floatTabbed :: (Show a, Eq a, Shrinker s) => s -> Theme ->
               ModifiedLayout (Decoration TabBarDecoration s)
          (ModifiedLayout MouseResize (ModifiedLayout WindowArranger SimpleFloat)) a
floatTabbed s t = tabBar s t Top (mouseResize $ windowArrangeAll $ SF (decoHeight t))
