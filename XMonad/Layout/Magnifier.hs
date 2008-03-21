{-# OPTIONS_GHC -fglasgow-exts #-}
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
      MagnifyMsg (..)
    ) where

import XMonad
import XMonad.StackSet
import XMonad.Layout.LayoutModifier

-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Layout.Magnifier
--
-- Then edit your @layoutHook@ by adding the 'magnifier' layout modifier
-- to some layout:
--
-- > myLayouts = magnifier (Tall 1 (3/100) (1/2))  ||| Full ||| etc..
-- > main = xmonad defaultConfig { layoutHook = myLayouts }
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
-- >    , ((modMask x .|. controlMask              , xK_plus ), sendMessage MagnifyMore)
-- >    , ((modMask x .|. controlMask              , xK_minus), sendMessage MagnifyLess)
-- >    , ((modMask x .|. controlMask              , xK_o    ), sendMessage ToggleOff  )
-- >    , ((modMask x .|. controlMask .|. shiftMask, xK_o    ), sendMessage ToggleOn   )
--
-- For detailed instruction on editing the key binding see
-- "XMonad.Doc.Extending#Editing_key_bindings".

-- | Increase the size of the window that has focus
magnifier :: l a -> ModifiedLayout Magnifier l a
magnifier = ModifiedLayout (Mag 1.5 On All)

-- | Change the size of the window that has focus by a custom zoom
magnifiercz :: Rational -> l a -> ModifiedLayout Magnifier l a
magnifiercz cz = ModifiedLayout (Mag ((fromRational cz)*1.0::Double) On All)

-- | Increase the size of the window that has focus, unless if it is the
-- master window.
magnifier' :: l a -> ModifiedLayout Magnifier l a
magnifier' = ModifiedLayout (Mag 1.5 On NoMaster)

-- | Magnifier that defaults to Off
magnifierOff :: l a -> ModifiedLayout Magnifier l a
magnifierOff = ModifiedLayout (Mag 1.5 Off All)

-- | Increase the size of the window that has focus by a custom zoom,
-- unless if it is the master window.
magnifiercz' :: Rational -> l a -> ModifiedLayout Magnifier l a
magnifiercz' cz = ModifiedLayout (Mag ((fromRational cz)*1.0::Double) On NoMaster)

data MagnifyMsg = MagnifyMore | MagnifyLess | ToggleOn | ToggleOff deriving ( Typeable )
instance Message MagnifyMsg

data Magnifier a = Mag Zoom Toggle MagnifyMaster deriving (Read, Show)

type Zoom = Double

data Toggle        = On  | Off      deriving  (Read, Show)
data MagnifyMaster = All | NoMaster deriving  (Read, Show)

instance LayoutModifier Magnifier Window where
    redoLayout  (Mag z On All     ) = applyMagnifier z
    redoLayout  (Mag z On NoMaster) = unlessMaster $ applyMagnifier z
    redoLayout  _                   = nothing
        where nothing _ _ wrs = return (wrs, Nothing)

    handleMess (Mag z On  t) m
                    | Just MagnifyMore <- fromMessage m = return . Just $ (Mag (z + 0.1) On  t)
                    | Just MagnifyLess <- fromMessage m = return . Just $ (Mag (z - 0.1) On  t)
                    | Just ToggleOff   <- fromMessage m = return . Just $ (Mag (z      ) Off t)
    handleMess (Mag z Off t) m
                    | Just ToggleOn    <- fromMessage m = return . Just $ (Mag z         On  t)
    handleMess _ _ = return Nothing

    modifierDescription (Mag _ On  All     ) = "Magnifier"
    modifierDescription (Mag _ On  NoMaster) = "Magnifier NoMaster"
    modifierDescription (Mag _ Off _       ) = "Magnifier (off)"

type NewLayout a = Rectangle -> Stack a -> [(Window, Rectangle)] -> X ([(Window, Rectangle)], Maybe (Magnifier a))

unlessMaster :: NewLayout a -> NewLayout a
unlessMaster mainmod r s wrs = if null (up s) then return (wrs, Nothing)
                                              else mainmod r s wrs

applyMagnifier :: Double -> Rectangle -> t -> [(Window, Rectangle)] -> X ([(Window, Rectangle)], Maybe a)
applyMagnifier z r _ wrs = do focused <- withWindowSet (return . peek)
                              let mag (w,wr) ws | focused == Just w = ws ++ [(w, fit r $ magnify z wr)]
                                                | otherwise         = (w,wr) : ws
                              return (reverse $ foldr mag [] wrs, Nothing)

magnify :: Double -> Rectangle -> Rectangle
magnify zoom (Rectangle x y w h) = Rectangle x' y' w' h'
    where x' = x - fromIntegral (w' - w) `div` 2
          y' = y - fromIntegral (h' - h) `div` 2
          w' = round $ fromIntegral w * zoom
          h' = round $ fromIntegral h * zoom

fit :: Rectangle -> Rectangle -> Rectangle
fit (Rectangle sx sy sw sh) (Rectangle x y w h) = Rectangle x' y' w' h'
    where x' = max sx (x - (max 0 (x + fi w - sx - fi sw)))
          y' = max sy (y - (max 0 (y + fi h - sy - fi sh)))
          w' = min sw w
          h' = min sh h

fi :: (Num b, Integral a) => a -> b
fi = fromIntegral
