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
        maximizeRestore
    ) where

import XMonad
import XMonad.Layout.LayoutModifier
import Data.List ( partition )

-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Layout.Maximize
--
-- Then edit your @layoutHook@ by adding the Maximize layout modifier:
--
-- > myLayouts = maximize (Tall 1 (3/100) (1/2)) ||| Full ||| etc..
-- > main = xmonad defaultConfig { layoutHook = myLayouts }
--
-- For more detailed instructions on editing the layoutHook see:
--
-- "XMonad.Doc.Extending#Editing_the_layout_hook"
--
-- In the key-bindings, do something like:
--
-- >        , ((modMask x, xK_backslash), withFocused (sendMessage . maximizeRestore))
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
    redoLayout (Maximize mw) rect _ wrs = case mw of
        Just win ->
                return (maxed ++ rest, Nothing)
            where
                maxed = map (\(w, _) -> (w, maxRect)) toMax
                (toMax, rest) = partition (\(w, _) -> w == win) wrs
                maxRect = Rectangle (rect_x rect + 50) (rect_y rect + 50)
                    (rect_width rect - 100) (rect_height rect - 100)
        Nothing -> return (wrs, Nothing)
    handleMess (Maximize mw) m = case fromMessage m of
        Just (MaximizeRestore w) -> case mw of
            Just _ -> return $ Just $ Maximize Nothing
            Nothing -> return $ Just $ Maximize $ Just w
        _ -> return Nothing

-- vim: sw=4:et
