{-# LANGUAGE TypeSynonymInstances, MultiParamTypeClasses, DeriveDataTypeable, PatternGuards #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.Monitor
-- Copyright   :  (c) Roman Cheplyaka
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Roman Cheplyaka <roma@ro-che.info>
-- Stability   :  unstable
-- Portability :  unportable
--
-- Layout modfier for displaying some window (monitor) above other windows
--
-----------------------------------------------------------------------------
module XMonad.Layout.Monitor (
    -- * Usage
    -- $usage

    -- * Hints
    -- $hints

    -- * TODO
    -- $todo
    Property(..),
    MonitorMessage(..),
    addMonitor,
    addPersistentMonitor,
    addNamedMonitor,
    addNamedPersistentMonitor
    ) where

import XMonad
import XMonad.Layout.LayoutModifier
import XMonad.Util.WindowProperties
import Control.Monad

-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Layout.Monitor
--
-- Then add monitor to desired layouts:
--
-- > myLayouts = addMonitor (ClassName "Cairo-clock" `And` Title "MacSlow's Cairo-Clock") (Rectangle (1280-150) (800-150) 150 150) $ tall ||| Full ||| ...
-- 
-- And make the desired window unmanaged with ManageHook:
--
-- > , className =? "Cairo-clock"--> doIgnore
--
-- After that, if there exists a window with specified properties, it will be
-- displayed on top of all /tiled/ (not floated) windows on specified
-- position.
--
-- It's also useful to add some keybinding to toggle monitor visibility:
-- 
-- > , ((mod1Mask, xK_u     ), sendMessage ToggleMonitor)
--
-- Screenshot: <http://www.haskell.org/haskellwiki/Image:Xmonad-clock.png>

data Monitor a = Monitor {
    prop :: Property, -- a window which satisfies that property is chosen as monitor
    rect :: Rectangle, -- where to put monitor
    visible :: Bool, -- is it visible?
    mbName :: (Maybe String), -- name of monitor (useful when we have many of them)
    persistent :: Bool -- on all layouts?
    } deriving (Read, Show)

data MonitorMessage = ToggleMonitor | ShowMonitor | HideMonitor
                    | ToggleMonitorNamed String
                    | ShowMonitorNamed String
                    | HideMonitorNamed String
    deriving (Read,Show,Eq,Typeable)
instance Message MonitorMessage

withMonitor :: Property -> a -> (Window -> X a) -> X a
withMonitor p a fn = do
    monitorWindows <- allWithProperty p
    case monitorWindows of
        [] -> return a
        w:_ -> fn w

instance LayoutModifier Monitor Window where
    redoLayout mon _ _ rects = withMonitor (prop mon) (rects, Nothing) $ \w ->
        if visible mon
            then do tileWindow w (rect mon)
                    reveal w
                    return ((w,rect mon):rects, Nothing)
            else do hide w
                    return (rects, Nothing)
    handleMess mon mess
        | Just ToggleMonitor <- fromMessage mess = return $ Just $ mon { visible = not $ visible mon }
        | Just (ToggleMonitorNamed n) <- fromMessage mess = return $
            if mbName mon `elem` [Just n, Nothing] then Just $ mon { visible = not $ visible mon } else Nothing
        | Just ShowMonitor <- fromMessage mess = return $ Just $ mon { visible = True }
        | Just (ShowMonitorNamed n) <- fromMessage mess = return $
            if mbName mon `elem` [Just n, Nothing] then Just $ mon { visible = True } else Nothing
        | Just HideMonitor <- fromMessage mess = return $ Just $ mon { visible = False }
        | Just (HideMonitorNamed n) <- fromMessage mess = return $
            if mbName mon `elem` [Just n, Nothing] then Just $ mon { visible = False } else Nothing
        | Just Hide <- fromMessage mess = do unless (persistent mon) $ withMonitor (prop mon) () hide; return Nothing
        | otherwise = return Nothing
        
addMonitor :: Property -> Rectangle -> l a -> ModifiedLayout Monitor l a
addMonitor p r = ModifiedLayout (Monitor p r True Nothing False)
addPersistentMonitor :: Property -> Rectangle -> l a -> ModifiedLayout Monitor l a
addPersistentMonitor p r = ModifiedLayout (Monitor p r True Nothing True)
addNamedMonitor :: String -> Property -> Rectangle -> l a -> ModifiedLayout Monitor l a
addNamedMonitor name p r = ModifiedLayout (Monitor p r True (Just name) False)
addNamedPersistentMonitor :: String -> Property -> Rectangle -> l a -> ModifiedLayout Monitor l a
addNamedPersistentMonitor name p r = ModifiedLayout (Monitor p r True (Just name) True)

-- $hints
-- - This module assumes that there is only one window satisfying property exists.
--
-- - If you want the monitor to be available on /all/ layouts, use
-- 'addPersistentMonitor' instead of 'addMonitor' to avoid unnecessary
-- flickering. You can still toggle monitor with a keybinding.
--
-- - On the other hand, if you use the monitor only with some of the layouts, you
-- might want to hide it on the startup. Then change ManageHook to the following:
--
-- > className =? "Cairo-clock"--> (ask >>= \w -> liftX (hide w) >> doF (W.delete w))
--
-- - You can use several monitors with nested modifiers. Give them a name using
-- 'addNamedMonitor' or 'addNamedPersistentMonitor' to be able to toggle
-- them independently.
--
-- - You can display monitor only on specific workspaces with
-- "XMonad.Layout.PerWorkspace".

-- $todo
-- - make Monitor remember the window it manages
--
-- - automatically unmanage the window?
-- 
-- - specify position relative to the screen
--
-- - toggle monitor on all workspaces (how?)
