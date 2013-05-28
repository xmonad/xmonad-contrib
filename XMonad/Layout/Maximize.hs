{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.Maximize
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
        maximizeRestore,
        Maximize, MaximizeRestore,
    ) where

import XMonad
import qualified XMonad.StackSet as S
import XMonad.Layout.LayoutModifier
import Data.List ( partition )

-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Layout.Maximize
--
-- Then edit your @layoutHook@ by adding the Maximize layout modifier:
--
-- > myLayout = maximize (Tall 1 (3/100) (1/2)) ||| Full ||| etc..
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

data Maximize a = Maximize (Maybe Window) deriving ( Read, Show )
maximize :: LayoutClass l Window => l Window -> ModifiedLayout Maximize l Window
maximize = ModifiedLayout $ Maximize Nothing

data MaximizeRestore = MaximizeRestore Window deriving ( Typeable, Eq )
instance Message MaximizeRestore
maximizeRestore :: Window -> MaximizeRestore
maximizeRestore = MaximizeRestore

instance LayoutModifier Maximize Window where
    modifierDescription (Maximize _) = "Maximize"
    pureModifier (Maximize (Just target)) rect (Just (S.Stack focused _ _)) wrs =
            if focused == target
                then (maxed ++ rest, Nothing)
                else (rest ++ maxed, lay)
        where
            (toMax, rest) = partition (\(w, _) -> w == target) wrs
            maxed = map (\(w, _) -> (w, maxRect)) toMax
            maxRect = Rectangle (rect_x rect + 25) (rect_y rect + 25)
                (rect_width rect - 50) (rect_height rect - 50)
            lay | null maxed = Just (Maximize Nothing)
                | otherwise  = Nothing
    pureModifier _ _ _ wrs = (wrs, Nothing)

    pureMess (Maximize mw) m = case fromMessage m of
        Just (MaximizeRestore w) -> case mw of
            Just w' -> if (w == w')
                        then Just $ Maximize Nothing   -- restore window
                        else Just $ Maximize $ Just w  -- maximize different window
            Nothing -> Just $ Maximize $ Just w        -- maximize window
        _ -> Nothing

-- vim: sw=4:et
