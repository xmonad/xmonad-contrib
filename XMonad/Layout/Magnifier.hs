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
-- This is a layout modifier that will make a layout increase the size
-- of the window that has focus.
--
-- [Example screenshot using @magnifiercz' 1.3@ with one of the two stack windows focused.](https://user-images.githubusercontent.com/50166980/108524842-c5f69380-72cf-11eb-9fd6-b0bf67b13ed6.png)
--
-----------------------------------------------------------------------------


module XMonad.Layout.Magnifier
    ( -- * Usage
      -- $usage
      magnifier,
      magnifier',
      magnifierOff,
      maxMagnifierOff,
      magnifiercz,
      magnifiercz',
      maximizeVertical,
      MagnifyMsg (..),
      Magnifier,
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
-- > main = xmonad def { layoutHook = myLayout }
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
magnifier = ModifiedLayout (Mag 1 (1.5,1.5) On All)

-- | Change the size of the window that has focus by a custom zoom
magnifiercz :: Rational -> l a -> ModifiedLayout Magnifier l a
magnifiercz cz = ModifiedLayout (Mag 1 (fromRational cz, fromRational cz) On All)

-- | Increase the size of the window that has focus, unless if it is one of the
-- master windows.
magnifier' :: l a -> ModifiedLayout Magnifier l a
magnifier' = ModifiedLayout (Mag 1 (1.5,1.5) On NoMaster)

-- | Magnifier that defaults to Off
magnifierOff :: l a -> ModifiedLayout Magnifier l a
magnifierOff = ModifiedLayout (Mag 1 (1.5,1.5) Off All)

-- | A magnifier that greatly magnifies with defaults to Off
maxMagnifierOff :: l a -> ModifiedLayout Magnifier l a
maxMagnifierOff = ModifiedLayout (Mag 1 (1000,1000) Off All)

-- | Increase the size of the window that has focus by a custom zoom,
-- unless if it is one of the the master windows.
magnifiercz' :: Rational -> l a -> ModifiedLayout Magnifier l a
magnifiercz' cz = ModifiedLayout (Mag 1 (fromRational cz, fromRational cz) On NoMaster)

-- | A magnifier that greatly magnifies just the vertical direction
maximizeVertical :: l a -> ModifiedLayout Magnifier l a
maximizeVertical = ModifiedLayout (Mag 1 (1,1000) Off All)

data MagnifyMsg = MagnifyMore | MagnifyLess | ToggleOn | ToggleOff | Toggle deriving ( Typeable )
instance Message MagnifyMsg

data Magnifier a = Mag !Int (Double,Double) Toggle MagnifyMaster deriving (Read, Show)

data Toggle        = On  | Off      deriving  (Read, Show)
data MagnifyMaster = All | NoMaster deriving  (Read, Show)

instance LayoutModifier Magnifier Window where
    redoLayout  (Mag _ z On All     ) r (Just s) wrs = applyMagnifier z r s wrs
    redoLayout  (Mag n z On NoMaster) r (Just s) wrs = unlessMaster n (applyMagnifier z) r s wrs
    redoLayout  _                   _ _        wrs = return (wrs, Nothing)

    handleMess (Mag n z On  t) m
                    | Just MagnifyMore    <- fromMessage m = return . Just $ Mag n             (z `addto`   0.1 ) On  t
                    | Just MagnifyLess    <- fromMessage m = return . Just $ Mag n             (z `addto` (-0.1)) On  t
                    | Just ToggleOff      <- fromMessage m = return . Just $ Mag n             z                  Off t
                    | Just Toggle         <- fromMessage m = return . Just $ Mag n             z                  Off t
                    | Just (IncMasterN d) <- fromMessage m = return . Just $ Mag (max 0 (n+d)) z                  On  t
                    where addto (x,y) i = (x+i,y+i)
    handleMess (Mag n z Off t) m
                    | Just ToggleOn       <- fromMessage m = return . Just $ Mag n             z                  On  t
                    | Just Toggle         <- fromMessage m = return . Just $ Mag n             z                  On  t
                    | Just (IncMasterN d) <- fromMessage m = return . Just $ Mag (max 0 (n+d)) z                  Off t
    handleMess _ _ = return Nothing

    modifierDescription (Mag _ _ On  All     ) = "Magnifier"
    modifierDescription (Mag _ _ On  NoMaster) = "Magnifier NoMaster"
    modifierDescription (Mag _ _ Off _       ) = "Magnifier (off)"

type NewLayout a = Rectangle -> Stack a -> [(Window, Rectangle)] -> X ([(Window, Rectangle)], Maybe (Magnifier a))

unlessMaster :: Int -> NewLayout a -> NewLayout a
unlessMaster n mainmod r s wrs = if null (drop (n-1) (up s)) then return (wrs, Nothing)
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
