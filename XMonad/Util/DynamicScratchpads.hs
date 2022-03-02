-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Util.DynamicScratchpads
-- Description :  Dynamically declare any window as a scratchpad.
-- Copyright   :  (c) Robin Oberschweiber <mephory@mephory.com>
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Robin Obercshweiber <mephory@mephory.com>
-- Stability   :  unstable
-- Portability :  unportable
--
-- Dynamically declare any window as a scratchpad.
--
-----------------------------------------------------------------------------

module XMonad.Util.DynamicScratchpads {-# DEPRECATED "Use the dynamic scratchpad facility of XMonad.Util.NamedScratchpad instead." #-} (
  -- * Usage
  -- $usage
  makeDynamicSP,
  spawnDynamicSP
  ) where

import Graphics.X11.Types
import XMonad.Core
import XMonad.Operations
import qualified Data.Map as M
import qualified XMonad.StackSet as W
import qualified XMonad.Util.ExtensibleState as XS


-- $usage
-- Allows you to dynamically declare windows as scratchpads. You can bind a key
-- to make a window start/stop being a scratchpad, and another key to
-- spawn/hide that scratchpad.
--
-- Like with XMonad.Util.NamedScratchpad, you have to have a workspace called
-- NSP, where hidden scratchpads will be moved to.
--
-- You can declare dynamic scratchpads in your xmonad.hs like so:
--
-- import XMonad.Util.DynamicScratchpads
--
-- , ((modm .|. shiftMask, xK_a), withFocused $ makeDynamicSP "dyn1")
-- , ((modm .|. shiftMask, xK_b), withFocused $ makeDynamicSP "dyn2")
-- , ((modm              , xK_a), spawnDynamicSP "dyn1")
-- , ((modm              , xK_b), spawnDynamicSP "dyn2")

-- | Stores dynamic scratchpads as a map of name to window
newtype SPStorage = SPStorage (M.Map String Window)
    deriving (Read,Show)

instance ExtensionClass SPStorage where
    initialValue = SPStorage M.empty
    extensionType = PersistentExtension

-- | Makes a window a dynamic scratchpad with the given name, or stop a window
-- | from being a dynamic scratchpad, if it already is.
makeDynamicSP :: String -- ^ Scratchpad name
              -> Window -- ^ Window to be made a scratchpad
              -> X ()
makeDynamicSP s w = do
    (SPStorage m) <- XS.get
    case M.lookup s m of
        Nothing -> addDynamicSP s w
        Just ow  -> if w == ow
                    then removeDynamicSP s
                    else showWindow ow >> addDynamicSP s w
{-# DEPRECATED makeDynamicSP "Use XMonad.Util.NamedScratchpad.toggleDynamicNSP instead" #-}

-- | Spawn the specified dynamic scratchpad
spawnDynamicSP :: String -- ^ Scratchpad name
               -> X ()
spawnDynamicSP s = do
    (SPStorage m) <- XS.get
    maybe mempty spawnDynamicSP' (M.lookup s m)
{-# DEPRECATED spawnDynamicSP "Use XMonad.Util.NamedScratchpad.dynamicNSPAction instead" #-}

spawnDynamicSP' :: Window -> X ()
spawnDynamicSP' w = withWindowSet $ \s -> do
    let matchingWindows = filter (== w) ((maybe [] W.integrate . W.stack . W.workspace . W.current) s)
    case matchingWindows of
        [] -> showWindow w
        _  -> hideWindow w

-- | Make a window a dynamic scratchpad
addDynamicSP :: String -> Window -> X ()
addDynamicSP s w = XS.modify $ alterSPStorage (\_ -> Just w) s

-- | Make a window stop being a dynamic scratchpad
removeDynamicSP :: String -> X ()
removeDynamicSP s = XS.modify $ alterSPStorage (const Nothing) s

-- | Moves window to the scratchpad workspace, effectively hiding it
hideWindow :: Window -> X ()
hideWindow = windows . W.shiftWin "NSP"

-- | Move window to current workspace and focus it
showWindow :: Window -> X ()
showWindow w = windows $ \ws ->
    W.focusWindow w . W.shiftWin (W.currentTag ws) w $ ws

alterSPStorage :: (Maybe Window -> Maybe Window) -> String -> SPStorage -> SPStorage
alterSPStorage f k (SPStorage m) = SPStorage $ M.alter f k m

-- vim:ts=4:shiftwidth=4:softtabstop=4:expandtab:
