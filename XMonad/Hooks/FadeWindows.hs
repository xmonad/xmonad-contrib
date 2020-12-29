{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Hooks.FadeWindows
-- Copyright   :  Brandon S Allbery KF8NH <allbery.b@gmail.com>
-- License     :  BSD
--
-- Maintainer  :  Brandon S Allbery KF8NH
-- Stability   :  unstable
-- Portability :  unportable
--
-- A more flexible and general compositing interface than FadeInactive.
-- Windows can be selected and opacity specified by means of FadeHooks,
-- which are very similar to ManageHooks and use the same machinery.
--
-----------------------------------------------------------------------------

module XMonad.Hooks.FadeWindows (-- * Usage
                                 -- $usage

                                 -- * The 'logHook' for window fading
                                 fadeWindowsLogHook

                                 -- * The 'FadeHook'
                                ,FadeHook
                                ,Opacity
                                ,idFadeHook

                                 -- * Predefined 'FadeHook's
                                ,opaque
                                ,solid
                                ,transparent
                                ,invisible
                                ,transparency
                                ,translucence
                                ,fadeBy
                                ,opacity
                                ,fadeTo

                                -- * 'handleEventHook' for mapped/unmapped windows
                                ,fadeWindowsEventHook

                                -- * 'doF' for simple hooks
                                ,doS

                                -- * Useful 'Query's for 'FadeHook's
                                ,isFloating
                                ,isUnfocused
                                ) where

import           XMonad.Core
import           XMonad.ManageHook                       (liftX)
import qualified XMonad.StackSet             as W

import           XMonad.Hooks.FadeInactive               (setOpacity
                                                         ,isUnfocused
                                                         )

import           Control.Monad                           (forM_)
import           Control.Monad.Reader                    (ask
                                                         ,asks)
import           Control.Monad.State                     (gets)
import qualified Data.Map                    as M
import           Data.Monoid

import           Graphics.X11.Xlib.Extras                (Event(..))

-- $usage
-- To use this module, make sure your @xmonad@ core supports generalized
-- 'ManageHook's (check the type of 'idHook'; if it's @ManageHook@ then
-- your @xmonad@ is too old) and then add @fadeWindowsLogHook@ to your
-- 'logHook' and @fadeWindowsEventHook@ to your 'handleEventHook':
--
-- >     , logHook = fadeWindowsLogHook myFadeHook
-- >     , handleEventHook = fadeWindowsEventHook
-- >     {- ... -}
-- >
-- > myFadeHook = composeAll [                 opaque
-- >                         , isUnfocused --> transparency 0.2
-- >                         ]
--
-- The above is like FadeInactive with a fade value of 0.2.
--
-- 'FadeHook's do not accumulate; instead, they compose from right to
-- left like 'ManageHook's, so in the above example @myFadeHook@ will
-- render unfocused windows at 4/5 opacity and the focused window as
-- opaque.  This means that, in particular, the order in the above
-- example is important.
--
-- The 'opaque' hook above is optional, by the way, as any unmatched
-- window will be opaque by default.  If you want to make all windows a
-- bit transparent by default, you can replace 'opaque' with something
-- like
--
-- > transparency 0.93
--
-- at the top of @myFadeHook@.
--
-- This module is best used with "XMonad.Hooks.MoreManageHelpers", which
-- exports a number of Queries that can be used in either @ManageHook@
-- or @FadeHook@.
--
-- Note that you need a compositing manager such as @xcompmgr@,
-- @dcompmgr@, or @cairo-compmgr@ for window fading to work.  If you
-- aren't running a compositing manager, the opacity will be recorded
-- but won't take effect until a compositing manager is started.
--
-- For more detailed instructions on editing the 'logHook' see:
--
-- "XMonad.Doc.Extending#The_log_hook_and_external_status_bars"
--
-- For more detailed instructions on editing the 'handleEventHook',
-- see:
--
-- "XMonad.Doc.Extending#Editing_the_event_hook"
-- (which sadly doesnt exist at the time of writing...)
--
-- /WARNING:/  This module is very good at triggering bugs in
-- compositing managers.  Symptoms range from windows not being
-- repainted until the compositing manager is restarted or the
-- window is unmapped and remapped, to the machine becoming sluggish
-- until the compositing manager is restarted (at which point a
-- popup/dialog will suddenly appear; apparently it's getting into
-- a tight loop trying to fade the popup in).  I find it useful to
-- have a key binding to restart the compositing manager; for example,
--
-- main = xmonad $ def {
--                   {- ... -}
--                 }
--                 `additionalKeysP`
--                 [("M-S-4",spawn "killall xcompmgr; sleep 1; xcompmgr -cCfF &")]
--                 {- ... -}
--                 ]
--
-- (See "XMonad.Util.EZConfig" for 'additionalKeysP'.)

-- a window opacity to be carried in a Query.  OEmpty is sort of a hack
-- to make it obay the monoid laws
data Opacity = Opacity Rational | OEmpty

instance Monoid Opacity where
  mempty                  = OEmpty
  r      `mappend` OEmpty = r
  _      `mappend` r      = r

instance Semigroup Opacity where
  (<>) = mappend

-- | A FadeHook is similar to a ManageHook, but records window opacity.
type FadeHook = Query Opacity

-- | Render a window fully opaque.
opaque :: FadeHook
opaque =  doS (Opacity 1)

-- | Render a window fully transparent.
transparent :: FadeHook
transparent =  doS (Opacity 0)

-- | Specify a window's transparency.
transparency :: Rational -- ^ The window's transparency as a fraction.
                         --   @transparency 1@ is the same as 'transparent',
                         --   whereas @transparency 0@ is the same as 'opaque'.
             -> FadeHook
transparency =  doS . Opacity . (1-) . clampRatio

-- | Specify a window's opacity; this is the inverse of 'transparency'.
opacity :: Rational -- ^ The opacity of a window as a fraction.
                    --   @opacity 1@ is the same as 'opaque',
                    --   whereas @opacity 0@ is the same as 'transparent'.
        -> FadeHook
opacity =  doS . Opacity . clampRatio

fadeTo, translucence, fadeBy :: Rational -> FadeHook
-- ^ An alias for 'transparency'.
fadeTo       = transparency
-- ^ An alias for 'transparency'.
translucence = transparency
-- ^ An alias for 'transparency'.
fadeBy       = opacity

invisible, solid :: FadeHook
-- ^ An alias for 'transparent'.
invisible    = transparent
-- ^ An alias for 'opaque'.
solid        = opaque

-- | Like 'doF', but usable with 'ManageHook'-like hooks that
-- aren't 'Query' wrapped around transforming functions ('Endo').
doS :: Monoid m => m -> Query m
doS =  return

-- | The identity 'FadeHook', which renders windows 'opaque'.
idFadeHook :: FadeHook
idFadeHook =  opaque

-- | A Query to determine if a window is floating.
isFloating :: Query Bool
isFloating =  ask >>= \w -> liftX . gets $ M.member w . W.floating . windowset

-- boring windows can't be seen outside of a layout, so we watch messages with
-- a dummy LayoutModifier and stow them in a persistent bucket.  this is not
-- entirely reliable given that boringAuto still isn't observable; we just hope
-- those aren't visible and won;t be affected anyway
-- @@@ punted for now, will be a separate module.  it's still slimy, though

-- | A 'logHook' to fade windows under control of a 'FadeHook', which is
--   similar to but not identical to 'ManageHook'.
fadeWindowsLogHook   :: FadeHook -> X ()
fadeWindowsLogHook h =  withWindowSet $ \s -> do
  let visibleWins = (W.integrate' . W.stack . W.workspace . W.current $ s) ++
                    concatMap (W.integrate' . W.stack . W.workspace) (W.visible s)
  forM_ visibleWins $ \w -> do
    o <- userCodeDef (Opacity 1) (runQuery h w)
    setOpacity w $ case o of
                     OEmpty    -> 1
                     Opacity r -> r

-- | A 'handleEventHook' to handle fading and unfading of newly mapped
--   or unmapped windows; this avoids problems with layouts such as
--   "XMonad.Layout.Full" or "XMonad.Layout.Tabbed".  This hook may
--   also be useful with "XMonad.Hooks.FadeInactive".
fadeWindowsEventHook                     :: Event -> X All
fadeWindowsEventHook (MapNotifyEvent {}) =
  -- we need to run the fadeWindowsLogHook.  only one way...
  asks config >>= logHook >> return (All True)
fadeWindowsEventHook _                   =  return (All True)

-- A utility to clamp opacity fractions to the range (0,1)
clampRatio   :: Rational         -> Rational
clampRatio r |  r >= 0 && r <= 1 =  r
             |  r < 0            =  0
             |  otherwise        =  1
