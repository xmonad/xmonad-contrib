{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE PatternGuards #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.FixedAspectRatio
-- Description :  A layout modifier for user provided per-window aspect ratios.
-- Copyright   :  (c) Yecine Megdiche <yecine.megdiche@gmail.com>
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  Yecine Megdiche <yecine.megdiche@gmail.com>
-- Stability   :  unstable
-- Portability :  unportable
--
-- Layout modifier for user provided per-window aspect ratios.
--
-----------------------------------------------------------------------------

module XMonad.Layout.FixedAspectRatio
  (
    -- * Usage
    -- $usage
    fixedAspectRatio
  , FixedAspectRatio
  , ManageAspectRatio(..)
  , doFixAspect
  ) where


import           Control.Arrow
import qualified Data.Map                      as M
import           Data.Ratio

import           XMonad
import           XMonad.Actions.MessageFeedback
import           XMonad.Layout.Decoration
import           XMonad.Layout.LayoutHints

-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Layout.FixedAspectRatio
-- Then add it to your layout:
--
-- > myLayout = fixedAspectRatio (0.5, 0.5) $ Tall 1 (3/100) (1/2)  ||| Full ||| etc..
-- > main = xmonad def { layoutHook = myLayout }
--
-- Which will center the (eventually) shrinked windows in their assigned
-- rectangle.
--
-- For a layout modifier that automatically sets the aspect ratio
-- depending on the size hints (for example for programs like mpv),
-- see "XMonad.Layout.LayoutHints"
--
-- See "XMonad.Doc.Extending#Editing_the_layout_hook" for more info on
-- the 'layoutHook'.
--
-- You also want to add keybindings to set and clear the aspect ratio:
--
-- >      -- Set the aspect ratio of the focused window to 16:9
-- >   ,((modm, xK_a), withFocused $ sendMessage . FixRatio (16 / 9))
-- >
-- >      -- Clear the aspect ratio from the focused window
-- >   ,((modm .|. shiftMask, xK_a), withFocused $ sendMessage . ResetRatio)
--
-- There's one caveat: to keep the usage of the modifier simple, it
-- doesn't remove a window from its cache automatically. Which means
-- that if you close a program window that has some fixed aspect ratios
-- and relaunch it, sometimes it'll still have the fixed aspect ratio.
-- You can try to avoid this by changing they keybinding used to kill
-- the window:
--
-- >  , ((modMask .|. shiftMask, xK_c), withFocused (sendMessage . ResetRatio) >> kill)
--
-- See "XMonad.Doc.Extending#Editing_key_bindings" for more info
-- on customizing the keybindings.
--
-- This layout also comes with a 'ManageHook' 'doFixAspect' to
-- automatically fix the aspect ratio:
--
-- > myManageHook = composeOne [
-- >   title =? "Netflix" <||> className =? "vlc" --> doFixAspect (16 / 9)
-- >   ...
-- > ]
--
-- Check "XMonad.Doc.Extending#Editing_the_manage_hook" for more information on
-- customizing the manage hook.

-- | Similar to 'layoutHintsWithReplacement', but relies on the user to
-- provide the ratio for each window. @aspectRatio (rx, ry) layout@ will
-- adapt the sizes of a layout's windows according to the provided aspect
-- ratio, and position them inside their originally assigned area
-- according to the @rx@ and @ry@ parameters.
-- (0, 0) places the window at the top left, (1, 0) at the top right,
-- (0.5, 0.5) at the center, etc.
fixedAspectRatio
  :: (Double, Double) -> l a -> ModifiedLayout FixedAspectRatio l a
fixedAspectRatio = ModifiedLayout . FixedAspectRatio mempty

data FixedAspectRatio a = FixedAspectRatio (M.Map Window Rational)
                                           (Double, Double)
  deriving (Read, Show)

instance LayoutModifier FixedAspectRatio Window where
  -- | Note: this resembles redoLayout from "XMonad.Layout.LayoutHints".
  -- The only difference is relying on user defined aspect ratios, and
  -- using the 'adj' function defined below instead of 'mkAdjust'
  pureModifier (FixedAspectRatio ratios placement) _ (Just s) xs =
    (xs', Nothing)
   where
    xs' =
      map (\x@(_, r) -> second (placeRectangle placement r) $ applyHint x) xs
    applyHint (win, r@(Rectangle x y w h)) =
      let ar       = M.lookup win ratios
          (w', h') = maybe (w, h) (adj (w, h)) ar
      in  (win, if isInStack s win then Rectangle x y w' h' else r)

  pureModifier _ _ _ xs = (xs, Nothing)

  handleMess (FixedAspectRatio ratios placement) mess
    | Just DestroyWindowEvent { ev_window = w } <- fromMessage mess
    = return . Just $ FixedAspectRatio (deleted w) placement
    | otherwise
    = case fromMessage mess of
      Just (FixRatio r w) ->
        return . Just $ FixedAspectRatio (inserted w r) placement
      Just (ResetRatio w) ->
        return . Just $ FixedAspectRatio (deleted w) placement
      Just (ToggleRatio r w) ->
        return
          . Just
          . flip FixedAspectRatio placement
          . maybe (inserted w r) (const $ deleted w)
          $ M.lookup w ratios
      _ -> return Nothing
   where
    inserted w r = M.insert w r ratios
    deleted w = M.delete w ratios

-- | A 'ManageHook' to set the aspect ratio for newly spawned windows
doFixAspect
  :: Rational -- ^ The aspect ratio
  -> ManageHook
doFixAspect r = ask
  >>= \w -> liftX (sendMessageWithNoRefreshToCurrent (FixRatio r w)) >> mempty

-- | Calculates the new width and height so they respect the
-- aspect ratio.
adj :: (Dimension, Dimension) -> Rational -> (Dimension, Dimension)
adj (w, h) ar | ar' > ar  = (ceiling $ fi h * ar, h)
              | otherwise = (w, ceiling $ fi w / ar)
  where ar' = fi w % fi h

--- Message handling
data ManageAspectRatio =
    FixRatio Rational Window    -- ^ Set the aspect ratio for the window
  | ResetRatio Window           -- ^ Remove the aspect ratio for the window
  | ToggleRatio Rational Window -- ^ Toggle the reatio
  deriving Typeable

instance Message ManageAspectRatio
