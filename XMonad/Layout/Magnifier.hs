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
      MagnifyMsg (..)
    ) where

import Graphics.X11.Xlib (Window, Rectangle(..))
import XMonad
import XMonad.StackSet
import XMonad.Layout.LayoutModifier

-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Layout.Magnifier
--
-- Then edit your @layoutHook@ by adding the Magnifier layout modifier
-- to some layout:
--
-- > myLayouts = magnifier (Tall 1 (3/100) (1/2))  ||| Full ||| etc..
-- > main = xmonad dafaultConfig { layoutHook = myLayouts }
--
-- For more detailed instructions on editing the layoutHook see:
--
-- "XMonad.Doc.Extending#Editing_the_layout_hook"
--
-- Magnifier supports some commands. To used them add something like
-- that to your key bindings:
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

-- | Increase the size of the window that has focus, unless if it is the
-- master window.
magnifier' :: l a -> ModifiedLayout Magnifier l a
magnifier' = ModifiedLayout (Mag 1.5 On NoMaster)

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
                    | Just ToggleOff   <- fromMessage m = return . Just $ (Mag (z + 0.1) Off t)
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
                              let mag (w,wr) ws | focused == Just w = ws ++ [(w, shrink r $ magnify z wr)]
                                                | otherwise         = (w,wr) : ws
                              return (reverse $ foldr mag [] wrs, Nothing)

magnify :: Double -> Rectangle -> Rectangle
magnify zoom (Rectangle x y w h) = Rectangle x' y' w' h'
    where x' = x - fromIntegral (w' - w) `div` 2
          y' = y - fromIntegral (h' - h) `div` 2
          w' = round $ fromIntegral w * zoom
          h' = round $ fromIntegral h * zoom

shrink :: Rectangle -> Rectangle -> Rectangle
shrink (Rectangle sx sy sw sh) (Rectangle x y w h) = Rectangle x' y' w' h'
    where x' = max sx x
          y' = max sy y
          w' = min w (fromIntegral sx + sw - fromIntegral x')
          h' = min h (fromIntegral sy + sh - fromIntegral y')
