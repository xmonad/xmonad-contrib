-- |
-- Module      :  XMonad.Actions.ToggleFullFloat
-- Description :  Fullscreen (float) a window while remembering its original state.
-- Copyright   :  (c) 2022 Tomáš Janoušek <tomi@nomi.cz>
-- License     :  BSD3
-- Maintainer  :  Tomáš Janoušek <tomi@nomi.cz>
--
module XMonad.Actions.ToggleFullFloat (
    -- * Usage
    -- $usage
    toggleFullFloatEwmhFullscreen,
    toggleFullFloat,
    fullFloat,
    unFullFloat,
    gcToggleFullFloat,
    ) where

import qualified Data.Map.Strict as M

import XMonad
import XMonad.Prelude
import XMonad.Hooks.EwmhDesktops (setEwmhFullscreenHooks)
import XMonad.Hooks.ManageHelpers
import qualified XMonad.StackSet as W
import qualified XMonad.Util.ExtensibleState as XS

-- ---------------------------------------------------------------------
-- $usage
--
-- The main use-case is to make 'ewmhFullscreen' (re)store the size and
-- position of floating windows instead of just unconditionally sinking them
-- into the floating layer. To enable this, you'll need this in your
-- @xmonad.hs@:
--
-- > import XMonad
-- > import XMonad.Actions.ToggleFullFloat
-- > import XMonad.Hooks.EwmhDesktops
-- >
-- > main = xmonad $ … . toggleFullFloatEwmhFullscreen . ewmhFullscreen . ewmh . … $ def{…}
--
-- Additionally, this "smart" fullscreening can be bound to a key and invoked
-- manually whenever one needs a larger window temporarily:
--
-- >   , ((modMask .|. shiftMask, xK_t), withFocused toggleFullFloat)

newtype ToggleFullFloat = ToggleFullFloat{ fromToggleFullFloat :: M.Map Window (Maybe W.RationalRect) }
    deriving (Show, Read)

instance ExtensionClass ToggleFullFloat where
    extensionType = PersistentExtension
    initialValue = ToggleFullFloat mempty

-- | Full-float a window, remembering its state (tiled/floating and
-- position/size).
fullFloat :: Window -> X ()
fullFloat = windows . appEndo <=< runQuery doFullFloatSave

-- | Restore window to its remembered state.
unFullFloat :: Window -> X ()
unFullFloat = windows . appEndo <=< runQuery doFullFloatRestore

-- | Full-float a window, if it's not already full-floating. Otherwise,
-- restore its original state.
toggleFullFloat :: Window -> X ()
toggleFullFloat w = ifM (isFullFloat w) (unFullFloat w) (fullFloat w)

isFullFloat :: Window -> X Bool
isFullFloat w = gets $ (Just fullRect ==) . M.lookup w . W.floating . windowset
  where
    fullRect = W.RationalRect 0 0 1 1

doFullFloatSave :: ManageHook
doFullFloatSave = do
    w <- ask
    liftX $ do
        f <- gets $ M.lookup w . W.floating . windowset
        -- @M.insertWith const@ = don't overwrite stored original state
        XS.modify' $ ToggleFullFloat . M.insertWith const w f . fromToggleFullFloat
    doFullFloat

doFullFloatRestore :: ManageHook
doFullFloatRestore = do
    w <- ask
    mf <- liftX $ do
        mf <- XS.gets $ M.lookup w . fromToggleFullFloat
        XS.modify' $ ToggleFullFloat . M.delete w . fromToggleFullFloat
        pure mf
    doF $ case mf of
        Just (Just f) -> W.float w f  -- was floating before
        Just Nothing -> W.sink w      -- was tiled before
        Nothing -> W.sink w           -- fallback when not found in ToggleFullFloat

-- | Install ToggleFullFloat garbage collection hooks.
--
-- Note: This is included in 'toggleFullFloatEwmhFullscreen', only needed if
-- using the 'toggleFullFloat' separately from the EWMH hook.
gcToggleFullFloat :: XConfig a -> XConfig a
gcToggleFullFloat c = c { startupHook     = startupHook c <> gcToggleFullFloatStartupHook
                        , handleEventHook = handleEventHook c <> gcToggleFullFloatEventHook }

-- | ToggleFullFloat garbage collection: drop windows when they're destroyed.
gcToggleFullFloatEventHook :: Event -> X All
gcToggleFullFloatEventHook DestroyWindowEvent{ev_window = w} = do
    XS.modify' $ ToggleFullFloat . M.delete w . fromToggleFullFloat
    mempty
gcToggleFullFloatEventHook _ = mempty

-- | ToggleFullFloat garbage collection: restrict to existing windows at
-- startup.
gcToggleFullFloatStartupHook :: X ()
gcToggleFullFloatStartupHook = withWindowSet $ \ws ->
    XS.modify' $ ToggleFullFloat . M.filterWithKey (\w _ -> w `W.member` ws) . fromToggleFullFloat

-- | Hook this module into 'XMonad.Hooks.EwmhDesktops.ewmhFullscreen'. This
-- makes windows restore their original state (size and position if floating)
-- instead of unconditionally sinking into the tiling layer.
--
-- ('gcToggleFullFloat' is included here.)
toggleFullFloatEwmhFullscreen :: XConfig a -> XConfig a
toggleFullFloatEwmhFullscreen =
    setEwmhFullscreenHooks doFullFloatSave doFullFloatRestore .
    gcToggleFullFloat
