{-# LANGUAGE MultiParamTypeClasses, DeriveDataTypeable, TypeSynonymInstances, FlexibleContexts #-}
----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.Minimize
-- Copyright   :  (c) Jan Vornberger 2009
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  jan.vornberger@informatik.uni-oldenburg.de
-- Stability   :  unstable
-- Portability :  not portable
--
-- Makes it possible to minimize windows, temporarily removing them
-- from the layout until they are restored.
--
-----------------------------------------------------------------------------

module XMonad.Layout.Minimize (
        -- * Usage
        -- $usage
        minimize,
        MinimizeMsg(..)
    ) where

import XMonad
import qualified XMonad.StackSet as W
import XMonad.Layout.LayoutModifier
import XMonad.Layout.BoringWindows as BW
import Data.List

-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Layout.Minimize
--
-- Then edit your @layoutHook@ by adding the Minimize layout modifier:
--
-- > myLayout = minimize (Tall 1 (3/100) (1/2)) ||| Full ||| etc..
-- > main = xmonad defaultConfig { layoutHook = myLayout }
--
-- For more detailed instructions on editing the layoutHook see:
--
-- "XMonad.Doc.Extending#Editing_the_layout_hook"
--
-- In the key-bindings, do something like:
--
-- >        , ((modm,               xK_m     ), withFocused (\f -> sendMessage (MinimizeWin f)))
-- >        , ((modm .|. shiftMask, xK_m     ), sendMessage RestoreNextMinimizedWin)
--
-- The first action will minimize the focused window, while the second one will restore
-- the next minimized window.
--
-- For detailed instruction on editing the key binding see:
--
-- "XMonad.Doc.Extending#Editing_key_bindings".
--
-- The module is designed to work together with "XMonad.Layout.BoringWindows" so
-- that minimized windows will be skipped when switching the focus window with
-- the keyboard.  Use the 'BW.boringAuto' function.
--
-- Also see "XMonad.Hooks.RestoreMinimized" if you want to be able to restore
-- minimized windows from your taskbar.

data Minimize a = Minimize [Window] deriving ( Read, Show )
minimize :: LayoutClass l Window => l Window -> ModifiedLayout Minimize l Window
minimize = ModifiedLayout $ Minimize []

data MinimizeMsg = MinimizeWin Window
                    | RestoreMinimizedWin Window
                    | RestoreNextMinimizedWin
                    deriving (Typeable, Eq)
instance Message MinimizeMsg

instance LayoutModifier Minimize Window where
    modifierDescription (Minimize _) = "Minimize"

    modifyLayout (Minimize minimized) wksp rect = do
        let stack = W.stack wksp
            filtStack = stack >>=W.filter (\w -> not (w `elem` minimized))
        runLayout (wksp {W.stack = filtStack}) rect

    handleMess (Minimize minimized) m = case fromMessage m of
        Just (MinimizeWin w)
          | not (w `elem` minimized) -> do
                BW.focusDown
                return $ Just $ Minimize (w:minimized)
          | otherwise               -> return Nothing
        Just (RestoreMinimizedWin w) ->
            return $ Just $ Minimize (minimized \\ [w])
        Just (RestoreNextMinimizedWin)
          | not (null minimized)    -> do
                focus (head minimized)
                return $ Just $ Minimize (tail minimized)
          | otherwise               -> return Nothing
        _ -> return Nothing
