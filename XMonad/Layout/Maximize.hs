{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.Maximize
-- Description :  Temporarily yank the focused window out of the layout to mostly fill the screen.
-- Copyright   :  (c) 2007 James Webb
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  xmonad#jwebb,sygneca,com
-- Stability   :  unstable
-- Portability :  unportable
--
-- Temporarily yanks the focused window out of the layout to mostly fill
-- the screen.
--
-----------------------------------------------------------------------------

module XMonad.Layout.Maximize (
        -- * Usage
        -- $usage
        maximize,
        maximizeWithPadding,
        maximizeRestore,
        Maximize, MaximizeRestore,
    ) where

import XMonad
import qualified XMonad.StackSet as S
import XMonad.Layout.LayoutModifier
import XMonad.Prelude ( partition )

-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Layout.Maximize
--
-- Then edit your @layoutHook@ by adding the Maximize layout modifier:
--
-- > myLayout = maximize (Tall 1 (3/100) (1/2)) ||| Full ||| etc..)
-- > main = xmonad def { layoutHook = myLayout }
--
-- Or, if you want to control the amount of padding placed around the
-- maximized window:
--
-- > myLayout = maximizeWithPadding 10 (Tall 1 (3/100) (1/2)) ||| Full ||| etc..)
-- > main = xmonad def { layoutHook = myLayout }
--
-- For more detailed instructions on editing the layoutHook see:
--
-- "XMonad.Doc.Extending#Editing_the_layout_hook"
--
-- In the key-bindings, do something like:
--
-- >        , ((modm, xK_backslash), withFocused (sendMessage . maximizeRestore))
-- >        ...
--
-- For detailed instruction on editing the key binding see:
--
-- "XMonad.Doc.Extending#Editing_key_bindings".

data Maximize a = Maximize Dimension (Maybe Window) deriving ( Read, Show )
maximize :: LayoutClass l Window => l Window -> ModifiedLayout Maximize l Window
maximize = ModifiedLayout $ Maximize 25 Nothing

-- | Like 'maximize', but allows you to specify the amount of padding
-- placed around the maximized window.
maximizeWithPadding :: LayoutClass l Window => Dimension -> l Window -> ModifiedLayout Maximize l Window
maximizeWithPadding padding = ModifiedLayout $ Maximize padding Nothing

newtype MaximizeRestore = MaximizeRestore Window deriving ( Eq )
instance Message MaximizeRestore
maximizeRestore :: Window -> MaximizeRestore
maximizeRestore = MaximizeRestore

instance LayoutModifier Maximize Window where
    modifierDescription (Maximize _ _) = "Maximize"
    pureModifier (Maximize padding (Just target)) rect (Just (S.Stack focused _ _)) wrs =
            if focused == target
                then (maxed ++ rest, Nothing)
                else (rest ++ maxed, lay)
        where
            (toMax, rest) = partition (\(w, _) -> w == target) wrs
            maxed = map (\(w, _) -> (w, maxRect)) toMax
            maxRect = Rectangle (rect_x rect + fromIntegral padding)
                                (rect_y rect + fromIntegral padding)
                                (rect_width rect  - padding * 2)
                                (rect_height rect - padding * 2)
            lay | null maxed = Just (Maximize padding Nothing)
                | otherwise  = Nothing
    pureModifier _ _ _ wrs = (wrs, Nothing)

    pureMess (Maximize padding mw) m = case fromMessage m of
        Just (MaximizeRestore w) -> case mw of
            Just w' -> if w == w'
                        then Just $ Maximize padding Nothing   -- restore window
                        else Just $ Maximize padding $ Just w  -- maximize different window
            Nothing -> Just $ Maximize padding $ Just w        -- maximize window
        _ -> Nothing

-- vim: sw=4:et
