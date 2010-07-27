{-# LANGUAGE MultiParamTypeClasses, DeriveDataTypeable, TypeSynonymInstances, FlexibleContexts, PatternGuards #-}
----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.Minimize
-- Copyright   :  (c) Jan Vornberger 2009, Alejandro Serrano 2010
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
        minimizeWindow,
        MinimizeMsg(RestoreMinimizedWin,RestoreNextMinimizedWin)
    ) where

import XMonad
import qualified XMonad.StackSet as W
import XMonad.Layout.LayoutModifier
import XMonad.Layout.BoringWindows as BW
import XMonad.Util.WindowProperties (getProp32)
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Foreign.C.Types (CLong)

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
-- >        , ((modm,               xK_m     ), withFocused minimizeWindow)
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
-- that minimized windows will be skipped over when switching the focused window with
-- the keyboard. Include 'BW.boringWindows' in your layout hook and see the
-- documentation of "XMonad.Layout.BoringWindows" on how to modify your keybindings.
--
-- Also see "XMonad.Hooks.Minimize" if you want to be able to minimize
-- and restore windows from your taskbar.

data Minimize a = Minimize [Window] (M.Map Window W.RationalRect) deriving ( Read, Show )
minimize :: LayoutClass l Window => l Window -> ModifiedLayout Minimize l Window
minimize = ModifiedLayout $ Minimize [] M.empty

data MinimizeMsg = MinimizeWin Window
                    | RestoreMinimizedWin Window
                    | RestoreNextMinimizedWin
                    deriving (Typeable, Eq)
instance Message MinimizeMsg

minimizeWindow :: Window -> X ()
minimizeWindow w = sendMessage (MinimizeWin w) >> BW.focusDown

setMinimizedState :: Window -> Int -> (CLong -> [CLong] -> [CLong]) -> X ()
setMinimizedState win st f = do
    setWMState win st
    withDisplay $ \dpy -> do
        state <- getAtom "_NET_WM_STATE"
        mini <- getAtom "_NET_WM_STATE_HIDDEN"
        wstate <- fromMaybe [] `fmap` getProp32 state win
        let ptype = 4 -- The atom property type for changeProperty
            fi_mini = fromIntegral mini
        io $ changeProperty32 dpy win state ptype propModeReplace (f fi_mini wstate)

setMinimized :: Window -> X ()
setMinimized win = setMinimizedState win iconicState (:)

setNotMinimized :: Window -> X ()
setNotMinimized win = setMinimizedState win normalState delete

instance LayoutModifier Minimize Window where
    modifierDescription _ = "Minimize"

    modifyLayout (Minimize minimized _) wksp rect = do
        let stack = W.stack wksp
            filtStack = stack >>=W.filter (\w -> not (w `elem` minimized))
        runLayout (wksp {W.stack = filtStack}) rect

    handleMess (Minimize minimized unfloated) m
        | Just (MinimizeWin w) <- fromMessage m, not (w `elem` minimized) = do
                setMinimized w
                ws <- gets windowset
                case M.lookup w (W.floating ws) of
                  Nothing -> return $ Just $ Minimize (w:minimized) unfloated
                  Just r -> do
                    modify (\s -> s { windowset = W.sink w ws})
                    return $ Just $ Minimize (w:minimized) (M.insert w r unfloated)
        | Just (RestoreMinimizedWin w) <- fromMessage m = do
            setNotMinimized w
            case M.lookup w unfloated of
              Nothing -> return $ Just $ Minimize (minimized \\ [w]) unfloated
              Just r -> do
                ws <- gets windowset
                modify (\s -> s { windowset = W.float w r ws})
                return $ Just $ Minimize (minimized \\ [w]) (M.delete w unfloated)
        | Just RestoreNextMinimizedWin <- fromMessage m = do
          ws <- gets windowset
          if not (null minimized)
            then case M.lookup (head minimized) unfloated of
              Nothing -> do
                let w = head minimized
                setNotMinimized w
                modify (\s -> s { windowset = W.focusWindow w ws})
                return $ Just $ Minimize (tail minimized) unfloated
              Just r -> do
                let w = head minimized
                setNotMinimized w
                modify (\s -> s { windowset = (W.focusWindow w . W.float w r) ws})
                return $ Just $ Minimize (tail minimized) (M.delete w unfloated)
            else return Nothing
        | Just BW.UpdateBoring <- fromMessage m = do
            ws <- gets (W.workspace . W.current . windowset)
            flip sendMessageWithNoRefresh ws $ BW.Replace "Minimize" minimized
            return Nothing
        | otherwise = return Nothing
