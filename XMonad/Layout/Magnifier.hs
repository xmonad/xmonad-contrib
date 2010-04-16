{-# LANGUAGE DeriveDataTypeable, MultiParamTypeClasses, TypeSynonymInstances, PatternGuards #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.Magnifier
-- Copyright   :  (c) Peter De Wachter and Andrea Rossato 2007
-- License     :  BSD-style (see xmonad/LICENSE)
--
-- Maintainer  :  andrea.rossato@unibz.it
-- Stability   :  unstable
-- Portability :  unportable
--
-- Screenshot  :  <http://caladan.rave.org/magnifier.png>
--
-- This is a layout modifier that will make a layout increase the size
-- of the window that has focus.
--
-----------------------------------------------------------------------------


module XMonad.Layout.Magnifier
    ( -- * Usage
      -- $usage
      magnifier,
      magnifier',
      magnifierOff,
      magnifiercz,
      magnifiercz',
      maximizeVertical,
      MagnifyMsg (..)
    ) where

import XMonad
import XMonad.StackSet
import XMonad.Layout.LayoutModifier
import XMonad.Util.XUtils

-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Layout.Magnifier
--
-- Then edit your @layoutHook@ by adding the 'magnifier' layout modifier
-- to some layout:
--
-- > myLayout = magnifier (Tall 1 (3/100) (1/2))  ||| Full ||| etc..
-- > main = xmonad defaultConfig { layoutHook = myLayout }
--
-- By default magnifier increases the focused window's size by 1.5.
-- You can also use:
--
-- > magnifiercz 1.2
--
-- to use a custom level of magnification.  You can even make the focused
-- window smaller for a pop in effect.
--
-- For more detailed instructions on editing the layoutHook see:
--
-- "XMonad.Doc.Extending#Editing_the_layout_hook"
--
-- Magnifier supports some commands. To use them add something like
-- this to your key bindings:
--
-- >    , ((modm .|. controlMask              , xK_plus ), sendMessage MagnifyMore)
-- >    , ((modm .|. controlMask              , xK_minus), sendMessage MagnifyLess)
-- >    , ((modm .|. controlMask              , xK_o    ), sendMessage ToggleOff  )
-- >    , ((modm .|. controlMask .|. shiftMask, xK_o    ), sendMessage ToggleOn   )
-- >    , ((modm .|. controlMask              , xK_m    ), sendMessage Toggle     )
--
-- Note that a few other extension modules, such as
-- "XMonad.Layout.MultiToggle" and "XMonad.Layout.ToggleLayouts", also
-- define a message named 'Toggle'.  To avoid conflicts when using
-- these modules together, you can import Magnifier qualified, like
-- this:
--
-- > import qualified XMonad.Layout.Magnifier as Mag
--
-- and then prefix @Mag@ to the front of everything from this module,
-- like @Mag.Toggle@, @Mag.magnifier@, and so on.
--
-- For detailed instruction on editing the key binding see
-- "XMonad.Doc.Extending#Editing_key_bindings".

-- | Increase the size of the window that has focus
magnifier :: l a -> ModifiedLayout Magnifier l a
magnifier = ModifiedLayout (Mag (1.5,1.5) On All)

-- | Change the size of the window that has focus by a custom zoom
magnifiercz :: Rational -> l a -> ModifiedLayout Magnifier l a
magnifiercz cz = ModifiedLayout (Mag (fromRational cz, fromRational cz) On All)

-- | Increase the size of the window that has focus, unless if it is the
-- master window.
magnifier' :: l a -> ModifiedLayout Magnifier l a
magnifier' = ModifiedLayout (Mag (1.5,1.5) On NoMaster)

-- | Magnifier that defaults to Off
magnifierOff :: l a -> ModifiedLayout Magnifier l a
magnifierOff = ModifiedLayout (Mag (1.5,1.5) Off All)

-- | Increase the size of the window that has focus by a custom zoom,
-- unless if it is the master window.
magnifiercz' :: Rational -> l a -> ModifiedLayout Magnifier l a
magnifiercz' cz = ModifiedLayout (Mag (fromRational cz, fromRational cz) On NoMaster)

-- | A magnifier that greatly magnifies just the vertical direction
maximizeVertical :: l a -> ModifiedLayout Magnifier l a
maximizeVertical = ModifiedLayout (Mag (1,1000) Off All)

data MagnifyMsg = MagnifyMore | MagnifyLess | ToggleOn | ToggleOff | Toggle deriving ( Typeable )
instance Message MagnifyMsg

data Magnifier a = Mag (Double,Double) Toggle MagnifyMaster deriving (Read, Show)

data Toggle        = On  | Off      deriving  (Read, Show)
data MagnifyMaster = All | NoMaster deriving  (Read, Show)

instance LayoutModifier Magnifier Window where
    redoLayout  (Mag z On All     ) r (Just s) wrs = applyMagnifier z r s wrs
    redoLayout  (Mag z On NoMaster) r (Just s) wrs = unlessMaster (applyMagnifier z) r s wrs
    redoLayout  _                   _ _        wrs = return (wrs, Nothing)

    handleMess (Mag z On  t) m
                    | Just MagnifyMore <- fromMessage m = return . Just $ (Mag (z `addto`   0.1 ) On  t)
                    | Just MagnifyLess <- fromMessage m = return . Just $ (Mag (z `addto` (-0.1)) On  t)
                    | Just ToggleOff   <- fromMessage m = return . Just $ (Mag (z      ) Off t)
                    | Just Toggle      <- fromMessage m = return . Just $ (Mag (z      ) Off t)
                    where addto (x,y) i = (x+i,y+i)
    handleMess (Mag z Off t) m
                    | Just ToggleOn    <- fromMessage m = return . Just $ (Mag z         On  t)
                    | Just Toggle      <- fromMessage m = return . Just $ (Mag z         On  t)
    handleMess _ _ = return Nothing

    modifierDescription (Mag _ On  All     ) = "Magnifier"
    modifierDescription (Mag _ On  NoMaster) = "Magnifier NoMaster"
    modifierDescription (Mag _ Off _       ) = "Magnifier (off)"

type NewLayout a = Rectangle -> Stack a -> [(Window, Rectangle)] -> X ([(Window, Rectangle)], Maybe (Magnifier a))

unlessMaster :: NewLayout a -> NewLayout a
unlessMaster mainmod r s wrs = if null (up s) then return (wrs, Nothing)
                                              else mainmod r s wrs

applyMagnifier :: (Double,Double) -> Rectangle -> t -> [(Window, Rectangle)]
               -> X ([(Window, Rectangle)], Maybe a)
applyMagnifier z r _ wrs = do focused <- withWindowSet (return . peek)
                              let mag (w,wr) ws | focused == Just w = ws ++ [(w, fit r $ magnify z wr)]
                                                | otherwise         = (w,wr) : ws
                              return (reverse $ foldr mag [] wrs, Nothing)

magnify :: (Double, Double) -> Rectangle -> Rectangle
magnify (zoomx,zoomy) (Rectangle x y w h) = Rectangle x' y' w' h'
    where x' = x - fromIntegral (w' - w) `div` 2
          y' = y - fromIntegral (h' - h) `div` 2
          w' = round $ fromIntegral w * zoomx
          h' = round $ fromIntegral h * zoomy

fit :: Rectangle -> Rectangle -> Rectangle
fit (Rectangle sx sy sw sh) (Rectangle x y w h) = Rectangle x' y' w' h'
    where x' = max sx (x - (max 0 (x + fi w - sx - fi sw)))
          y' = max sy (y - (max 0 (y + fi h - sy - fi sh)))
          w' = min sw w
          h' = min sh h
