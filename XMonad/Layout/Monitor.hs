{-# LANGUAGE TypeSynonymInstances, MultiParamTypeClasses, PatternGuards #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.Monitor
-- Description :  Layout modfier for displaying some window (monitor) above other windows.
-- Copyright   :  (c) Roman Cheplyaka
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Roman Cheplyaka <roma@ro-che.info>
-- Stability   :  unstable
-- Portability :  unportable
--
-- Layout modifier for displaying some window (monitor) above other windows.
--
-----------------------------------------------------------------------------
module XMonad.Layout.Monitor (
    -- * Usage
    -- $usage

    -- * Hints and issues
    -- $hints

    Monitor(..),
    monitor,
    Property(..),
    MonitorMessage(..),
    doHideIgnore,
    manageMonitor

    -- * TODO
    -- $todo
    ) where

import XMonad
import XMonad.Prelude (unless)
import XMonad.Layout.LayoutModifier
import XMonad.Util.WindowProperties
import XMonad.Hooks.ManageHelpers (doHideIgnore)
import XMonad.Hooks.FadeInactive (setOpacity)

-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Layout.Monitor
--
-- Define 'Monitor' record. 'monitor' can be used as a template. At least 'prop'
-- and 'rect' should be set here. Also consider setting 'persistent' to True.
--
-- Minimal example:
--
-- > myMonitor = monitor
-- >     { prop = ClassName "SomeClass"
-- >     , rect = Rectangle 0 0 40 20 -- rectangle 40x20 in upper left corner
-- >     }
--
-- More interesting example:
--
-- > clock = monitor {
-- >      -- Cairo-clock creates 2 windows with the same classname, thus also using title
-- >      prop = ClassName "Cairo-clock" `And` Title "MacSlow's Cairo-Clock"
-- >      -- rectangle 150x150 in lower right corner, assuming 1280x800 resolution
-- >    , rect = Rectangle (1280-150) (800-150) 150 150
-- >      -- avoid flickering
-- >    , persistent = True
-- >      -- make the window transparent
-- >    , opacity = 0.6
-- >      -- hide on start
-- >    , visible = False
-- >      -- assign it a name to be able to toggle it independently of others
-- >    , name = "clock"
-- >    }
--
-- Add ManageHook to de-manage monitor windows and apply opacity settings.
--
-- > manageHook = myManageHook <+> manageMonitor clock
--
-- Apply layout modifier.
--
-- > myLayout = ModifiedLayout clock $ tall ||| Full ||| ...
--
-- After that, if there exists a window with specified properties, it will be
-- displayed on top of all /tiled/ (not floated) windows on specified
-- position.
--
-- It's also useful to add some keybinding to toggle monitor visibility:
--
-- > , ((mod1Mask, xK_u     ), broadcastMessage ToggleMonitor >> refresh)
--
-- Screenshot: <http://www.haskell.org/haskellwiki/Image:Xmonad-clock.png>

data Monitor a = Monitor
    { prop :: Property    -- ^ property which uniquely identifies monitor window
    , rect :: Rectangle   -- ^ specifies where to put monitor
    , visible :: Bool     -- ^ is it visible by default?
    , name :: String      -- ^ name of monitor (useful when we have many of them)
    , persistent :: Bool  -- ^ is it shown on all layouts?
    , opacity :: Rational -- ^ opacity level
    } deriving (Read, Show)

-- | Template for 'Monitor' record. At least 'prop' and 'rect' should be
-- redefined. Default settings: 'visible' is 'True', 'persistent' is 'False'.
monitor :: Monitor a
monitor = Monitor
    { prop = Const False
    , rect = Rectangle 0 0 0 0
    , visible = True
    , name = ""
    , persistent = False
    , opacity = 1
    }

-- | Messages without names affect all monitors. Messages with names affect only
-- monitors whose names match.
data MonitorMessage = ToggleMonitor | ShowMonitor | HideMonitor
                    | ToggleMonitorNamed String
                    | ShowMonitorNamed String
                    | HideMonitorNamed String
    deriving (Read,Show,Eq)
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
            if name mon == n then Just $ mon { visible = not $ visible mon } else Nothing
        | Just ShowMonitor <- fromMessage mess = return $ Just $ mon { visible = True }
        | Just (ShowMonitorNamed n) <- fromMessage mess = return $
            if name mon == n then Just $ mon { visible = True } else Nothing
        | Just HideMonitor <- fromMessage mess = return $ Just $ mon { visible = False }
        | Just (HideMonitorNamed n) <- fromMessage mess = return $
            if name mon == n then Just $ mon { visible = False } else Nothing
        | Just Hide <- fromMessage mess = do unless (persistent mon) $ withMonitor (prop mon) () hide; return Nothing
        | otherwise = return Nothing

-- | ManageHook which demanages monitor window and applies opacity settings.
manageMonitor :: Monitor a -> ManageHook
manageMonitor mon = propertyToQuery (prop mon) --> do
    w <- ask
    liftX $ setOpacity w $ opacity mon
    if persistent mon then doIgnore else doHideIgnore

-- $hints
-- - This module assumes that there is only one window satisfying property exists.
--
-- - If your monitor is available on /all/ layouts, set
-- 'persistent' to 'True' to avoid unnecessary
-- flickering. You can still toggle monitor with a keybinding.
--
-- - You can use several monitors with nested modifiers. Give them names
---  to be able to toggle them independently.
--
-- - You can display monitor only on specific workspaces with
-- "XMonad.Layout.PerWorkspace".

-- $todo
-- - make Monitor remember the window it manages
--
-- - specify position relative to the screen
