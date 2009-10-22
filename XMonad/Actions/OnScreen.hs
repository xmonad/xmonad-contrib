-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Actions.OnScreen
-- Copyright   :  (c) 2009 Nils Schweinsberg
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  Nils Schweinsberg <mail@n-sch.de>
-- Stability   :  unstable
-- Portability :  unportable
--
-- Control workspaces on different screens (in xinerama mode).
--
-----------------------------------------------------------------------------

module XMonad.Actions.OnScreen (
    -- * Usage
    -- $usage
    onScreen
    , viewOnScreen
    , greedyViewOnScreen
    , onlyOnScreen
    ) where

import XMonad.StackSet
import Control.Monad(guard)
import Data.List
import Data.Maybe(fromMaybe)
import Data.Function(on)

-- $usage
--
-- This module provides an easy way to control, what you see on other screens in
-- xinerama mode without having to focus them. Put this into your
-- @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Actions.OnScreen
--
-- Then add the appropriate keybindings, for example replace your current keys
-- to switch the workspaces with this at the bottom of your keybindings:
--
-- >     ++
-- >     [ ((m .|. modm, k), windows (f i))
-- >       | (i, k) <- zip (workspaces conf) ([xK_1 .. xK_9] ++ [xK_0])
-- >       , (f, m) <- [ (viewOnScreen 0, 0)
-- >                   , (viewOnScreen 1, controlMask)
-- >                   , (greedyView, controlMask .|. shiftMask) ]
-- >     ]
--
-- This will provide you with the following keybindings:
--
--      * modkey + 1-0:
--      Switch to workspace 1-0 on screen 0
--
--      * modkey + control + 1-0:
--      Switch to workspace 1-0 on screen 1
--
--      * modkey + control + shift + 1-0:
--      Default greedyView behaviour
--
--
-- A more basic version inside the default keybindings would be:
--
-- >        , ((modm .|. controlMask, xK_1) windows (viewOnScreen 0 "1"))
--
-- where 0 is the first screen and "1" the workspace with the tag "1".
--
-- For detailed instructions on editing your key bindings, see
-- "XMonad.Doc.Extending#Editing_key_bindings".

-- | Switch to the (hidden) workspace with index 'i' on the screen 'sc'.
-- A default function (for example 'view' or 'greedyView') will be run if 'sc' is
-- the current screen, no valid screen id or workspace 'i' is already visible.
onScreen :: (Eq sid, Eq i)
          => (i -> StackSet i l a sid sd -> StackSet i l a sid sd) -- ^ default action
          -> sid                      -- ^ screen id
          -> i                        -- ^ index of the workspace
          -> StackSet i l a sid sd    -- ^ current stack
          -> StackSet i l a sid sd
onScreen defFunc sc i st = fromMaybe (defFunc i st) $ do
    -- on unfocused current screen
    guard $ screen (current st) /= sc
    x <- find ((i==) . tag    ) (hidden  st)
    s <- find ((sc==) . screen) (screens st)
    o <- find ((sc==) . screen) (visible st)
    let newScreen = s { workspace = x }
    return st { visible   = newScreen : deleteBy ((==) `on` screen) newScreen (visible st)
              , hidden    = workspace o : deleteBy ((==) `on` tag) x (hidden st)
              }

-- | Switch to workspace 'i' on screen 'sc'. If 'i' is visible use 'greedyView'
-- to switch the current workspace with workspace 'i'.
greedyViewOnScreen :: (Eq sid, Eq i)
    => sid                          -- ^ screen id
    -> i                            -- ^ index of the workspace
    -> StackSet i l a sid sd        -- ^ current stack
    -> StackSet i l a sid sd
greedyViewOnScreen = onScreen greedyView

-- | Switch to workspace 'i' on screen 'sc'. If 'i' is visible use 'view' to
-- switch focus to the workspace 'i'.
viewOnScreen :: (Eq sid, Eq i)
    => sid                          -- ^ screen id
    -> i                            -- ^ index of the workspace
    -> StackSet i l a sid sd        -- ^ current stack
    -> StackSet i l a sid sd
viewOnScreen = onScreen view

-- | Switch to workspace 'i' on screen 'sc'. If 'i' is visible do nothing.
onlyOnScreen :: (Eq sid, Eq i)
    => sid                          -- ^ screen id
    -> i                            -- ^ index of the workspace
    -> StackSet i l a sid sd        -- ^ current stack
    -> StackSet i l a sid sd
onlyOnScreen = onScreen doNothing
  where doNothing _ st = st
