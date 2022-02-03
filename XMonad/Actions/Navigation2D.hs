{-# LANGUAGE MultiParamTypeClasses, PatternGuards, RankNTypes, TypeSynonymInstances #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.Navigation2D
-- Description :  Directional navigation of windows and screens.
-- Copyright   :  (c) 2011  Norbert Zeh <nzeh@cs.dal.ca>
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  Norbert Zeh <nzeh@cs.dal.ca>
-- Stability   :  unstable
-- Portability :  unportable
--
-- Navigation2D is an xmonad extension that allows easy directional
-- navigation of windows and screens (in a multi-monitor setup).
-----------------------------------------------------------------------------

module XMonad.Actions.Navigation2D ( -- * Usage
                                     -- $usage

                                     -- * Finer points
                                     -- $finer_points

                                     -- * Alternative directional navigation modules
                                     -- $alternatives

                                     -- * Incompatibilities
                                     -- $incompatibilities

                                     -- * Detailed technical discussion
                                     -- $technical

                                     -- * Exported functions and types
                                     -- #Exports#

                                     navigation2D
                                   , navigation2DP
                                   , additionalNav2DKeys
                                   , additionalNav2DKeysP
                                   , withNavigation2DConfig
                                   , Navigation2DConfig(..)
                                   , def
                                   , Navigation2D
                                   , lineNavigation
                                   , centerNavigation
                                   , sideNavigation
                                   , sideNavigationWithBias
                                   , hybridOf
                                   , hybridNavigation
                                   , fullScreenRect
                                   , singleWindowRect
                                   , switchLayer
                                   , windowGo
                                   , windowSwap
                                   , windowToScreen
                                   , screenGo
                                   , screenSwap
                                   , Direction2D(..)
                                   ) where

import qualified Data.List as L
import qualified Data.Map as M
import Control.Arrow (second)
import XMonad.Prelude
import XMonad hiding (Screen)
import qualified XMonad.StackSet as W
import qualified XMonad.Util.ExtensibleState as XS
import XMonad.Util.EZConfig (additionalKeys, additionalKeysP)
import XMonad.Util.Types

-- $usage
-- #Usage#
-- Navigation2D provides directional navigation (go left, right, up, down) for
-- windows and screens.  It treats floating and tiled windows as two separate
-- layers and provides mechanisms to navigate within each layer and to switch
-- between layers.  Navigation2D provides three different navigation strategies
-- (see <#Technical_Discussion> for details): /Line navigation/ and
-- /Side navigation/ feel rather natural but may make it impossible to navigate
-- to a given window from the current window, particularly in the floating
-- layer. /Center navigation/ feels less natural in certain situations but
-- ensures that all windows can be reached without the need to involve the
-- mouse. Another option is to use a /Hybrid/ of the three strategies,
-- automatically choosing whichever first provides a suitable target window.
-- Navigation2D allows different navigation strategies to be used in the two
-- layers and allows customization of the navigation strategy for the tiled
-- layer based on the layout currently in effect.
--
-- You can use this module with (a subset of) the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Actions.Navigation2D
--
-- Then add the configuration of the module to your main function:
--
-- > main = xmonad $ navigation2D def
-- >                              (xK_Up, xK_Left, xK_Down, xK_Right)
-- >                              [(mod4Mask,               windowGo  ),
-- >                               (mod4Mask .|. shiftMask, windowSwap)]
-- >                              False
-- >               $ def
--
-- Alternatively, you can use navigation2DP:
--
-- > main = xmonad $ navigation2DP def
-- >                               ("<Up>", "<Left>", "<Down>", "<Right>")
-- >                               [("M-",   windowGo  ),
-- >                                ("M-S-", windowSwap)]
-- >                               False
-- >               $ def
--
-- That's it. If instead you'd like more control, you can combine
-- withNavigation2DConfig and additionalNav2DKeys or additionalNav2DKeysP:
--
-- > main = xmonad $ withNavigation2DConfig def
-- >               $ additionalNav2DKeys (xK_Up, xK_Left, xK_Down, xK_Right)
-- >                                     [(mod4Mask,               windowGo  ),
-- >                                      (mod4Mask .|. shiftMask, windowSwap)]
-- >                                     False
-- >               $ additionalNav2DKeys (xK_u, xK_l, xK_d, xK_r)
-- >                                     [(mod4Mask,               screenGo  ),
-- >                                      (mod4Mask .|. shiftMask, screenSwap)]
-- >                                     False
-- >               $ def
--
-- Or you can add the configuration of the module to your main function:
--
-- > main = xmonad $ withNavigation2DConfig def $ def
--
-- And specify your keybindings normally:
--
-- >    -- Switch between layers
-- >    , ((modm,                 xK_space), switchLayer)
-- >
-- >    -- Directional navigation of windows
-- >    , ((modm,                 xK_Right), windowGo R False)
-- >    , ((modm,                 xK_Left ), windowGo L False)
-- >    , ((modm,                 xK_Up   ), windowGo U False)
-- >    , ((modm,                 xK_Down ), windowGo D False)
-- >
-- >    -- Swap adjacent windows
-- >    , ((modm .|. controlMask, xK_Right), windowSwap R False)
-- >    , ((modm .|. controlMask, xK_Left ), windowSwap L False)
-- >    , ((modm .|. controlMask, xK_Up   ), windowSwap U False)
-- >    , ((modm .|. controlMask, xK_Down ), windowSwap D False)
-- >
-- >    -- Directional navigation of screens
-- >    , ((modm,                 xK_r    ), screenGo R False)
-- >    , ((modm,                 xK_l    ), screenGo L False)
-- >    , ((modm,                 xK_u    ), screenGo U False)
-- >    , ((modm,                 xK_d    ), screenGo D False)
-- >
-- >    -- Swap workspaces on adjacent screens
-- >    , ((modm .|. controlMask, xK_r    ), screenSwap R False)
-- >    , ((modm .|. controlMask, xK_l    ), screenSwap L False)
-- >    , ((modm .|. controlMask, xK_u    ), screenSwap U False)
-- >    , ((modm .|. controlMask, xK_d    ), screenSwap D False)
-- >
-- >    -- Send window to adjacent screen
-- >    , ((modm .|. mod1Mask,    xK_r    ), windowToScreen R False)
-- >    , ((modm .|. mod1Mask,    xK_l    ), windowToScreen L False)
-- >    , ((modm .|. mod1Mask,    xK_u    ), windowToScreen U False)
-- >    , ((modm .|. mod1Mask,    xK_d    ), windowToScreen D False)
--
-- For detailed instruction on editing the key binding see:
--
-- "XMonad.Doc.Extending#Editing_key_bindings".

-- $finer_points
-- #Finer_Points#
-- The above should get you started.  Here are some finer points:
--
-- Navigation2D has the ability to wrap around at screen edges.  For example, if
-- you navigated to the rightmost window on the rightmost screen and you
-- continued to go right, this would get you to the leftmost window on the
-- leftmost screen.  This feature may be useful for switching between screens
-- that are far apart but may be confusing at least to novice users.  Therefore,
-- it is disabled in the above example (e.g., navigation beyond the rightmost
-- window on the rightmost screen is not possible and trying to do so will
-- simply not do anything.)  If you want this feature, change all the 'False'
-- values in the above example to 'True'.  You could also decide you want
-- wrapping only for a subset of the operations and no wrapping for others.
--
-- By default, all layouts use the 'defaultTiledNavigation' strategy specified
-- in the 'Navigation2DConfig' (by default, line navigation is used).  To
-- override this behaviour for some layouts, add a pair (\"layout name\",
-- navigation strategy) to the 'layoutNavigation' list in the
-- 'Navigation2DConfig', where \"layout name\" is the string reported by the
-- layout's description method (normally what is shown as the layout name in
-- your status bar).  For example, all navigation strategies normally allow only
-- navigation between mapped windows.  The first step to overcome this, for
-- example, for the Full layout, is to switch to center navigation for the Full
-- layout:
--
-- > myNavigation2DConfig = def { layoutNavigation = [("Full", centerNavigation)] }
-- >
-- > main = xmonad $ withNavigation2DConfig myNavigation2DConfig
-- >               $ def
--
-- The navigation between windows is based on their screen rectangles, which are
-- available /and meaningful/ only for mapped windows.  Thus, as already said,
-- the default is to allow navigation only between mapped windows.  However,
-- there are layouts that do not keep all windows mapped.  One example is the
-- Full layout, which unmaps all windows except the one that has the focus,
-- thereby preventing navigation to any other window in the layout.  To make
-- navigation to unmapped windows possible, unmapped windows need to be assigned
-- rectangles to pretend they are mapped, and a natural way to do this for the
-- Full layout is to pretend all windows occupy the full screen and are stacked
-- on top of each other so that only the frontmost one is visible.  This can be
-- done as follows:
--
-- > myNavigation2DConfig = def { layoutNavigation   = [("Full", centerNavigation)]
-- >                            , unmappedWindowRect = [("Full", singleWindowRect)]
-- >                            }
-- >
-- > main = xmonad $ withNavigation2DConfig myNavigation2DConfig
-- >               $ def
--
-- With this setup, Left/Up navigation behaves like standard
-- 'XMonad.StackSet.focusUp' and Right/Down navigation behaves like
-- 'XMonad.StackSet.focusDown', thus allowing navigation between windows in the
-- layout.
--
-- In general, each entry in the 'unmappedWindowRect' association list is a pair
-- (\"layout description\", function), where the function computes a rectangle
-- for each unmapped window from the screen it is on and the window ID.
-- Currently, Navigation2D provides only two functions of this type:
-- 'singleWindowRect' and 'fullScreenRect'.
--
-- With per-layout navigation strategies, if different layouts are in effect on
-- different screens in a multi-monitor setup, and different navigation
-- strategies are defined for these active layouts, the most general of these
-- navigation strategies is used across all screens (because Navigation2D does
-- not distinguish between windows on different workspaces), where center
-- navigation is more general than line navigation, as discussed formally under
-- <#Technical_Discussion>.

-- $alternatives
-- #Alternatives#
--
-- There exist two alternatives to Navigation2D:
-- "XMonad.Actions.WindowNavigation" and "XMonad.Layout.WindowNavigation".
-- X.L.WindowNavigation has the advantage of colouring windows to indicate the
-- window that would receive the focus in each navigation direction, but it does
-- not support navigation across multiple monitors, does not support directional
-- navigation of floating windows, and has a very unintuitive definition of
-- which window receives the focus next in each direction.  X.A.WindowNavigation
-- does support navigation across multiple monitors but does not provide window
-- colouring while retaining the unintuitive navigational semantics of
-- X.L.WindowNavigation.  This makes it very difficult to predict which window
-- receives the focus next.  Neither X.A.WindowNavigation nor
-- X.L.WindowNavigation supports directional navigation of screens.

-- $technical
-- #Technical_Discussion#
-- An in-depth discussion of the navigational strategies implemented in
-- Navigation2D, including formal proofs of their properties, can be found
-- at <http://www.cs.dal.ca/~nzeh/xmonad/Navigation2D.pdf>.

-- $incompatibilities
-- #Incompatibilities#
-- Currently Navigation2D is known not to play nicely with tabbed layouts, but
-- it should work well with any other tiled layout.  My hope is to address the
-- incompatibility with tabbed layouts in a future version.  The navigation to
-- unmapped windows, for example in a Full layout, by assigning rectangles to
-- unmapped windows is more a workaround than a clean solution.  Figuring out
-- how to deal with tabbed layouts may also lead to a more general and cleaner
-- solution to query the layout for a window's rectangle that may make this
-- workaround unnecessary.  At that point, the 'unmappedWindowRect' field of the
-- 'Navigation2DConfig' will disappear.

-- | A rectangle paired with an object
type Rect a = (a, Rectangle)

-- | A shorthand for window-rectangle pairs.  Reduces typing.
type WinRect = Rect Window

-- | A shorthand for workspace-rectangle pairs.  Reduces typing.
type WSRect = Rect WorkspaceId

----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
--                                                                                                --
--                                        PUBLIC INTERFACE                                        --
--                                                                                                --
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

-- | Encapsulates the navigation strategy
data Navigation2D = N Generality (forall a . Eq a => Direction2D -> Rect a -> [Rect a] -> Maybe a)

runNav :: forall a . Eq a => Navigation2D -> (Direction2D -> Rect a -> [Rect a] -> Maybe a)
runNav (N _ nav) = nav

-- | Score that indicates how general a navigation strategy is
type Generality = Int

instance Eq Navigation2D where
  (N x _) == (N y _) = x == y

instance Ord Navigation2D where
  (N x _) <= (N y _) = x <= y

-- | Line navigation.  To illustrate this navigation strategy, consider
-- navigating to the left from the current window.  In this case, we draw a
-- horizontal line through the center of the current window and consider all
-- windows that intersect this horizontal line and whose right boundaries are to
-- the left of the left boundary of the current window.  From among these
-- windows, we choose the one with the rightmost right boundary.
lineNavigation :: Navigation2D
lineNavigation = N 1 doLineNavigation

-- | Center navigation.  Again, consider navigating to the left.  Then we
-- consider the cone bounded by the two rays shot at 45-degree angles in
-- north-west and south-west direction from the center of the current window.  A
-- window is a candidate to receive the focus if its center lies in this cone.
-- We choose the window whose center has minimum L1-distance from the current
-- window center.  The tie breaking strategy for windows with the same distance
-- is a bit complicated (see <#Technical_Discussion>) but ensures that all
-- windows can be reached and that windows with the same center are traversed in
-- their order in the window stack, that is, in the order
-- 'XMonad.StackSet.focusUp' and 'XMonad.StackSet.focusDown' would traverse
-- them.
centerNavigation :: Navigation2D
centerNavigation = N 2 doCenterNavigation

-- | Side navigation. Consider navigating to the right this time. The strategy
-- is to take the line segment forming the right boundary of the current window,
-- and push it to the right until it intersects with at least one other window.
-- Of those windows, one with a point that is the closest to the centre of the
-- line (+1) is selected. This is probably the most intuitive strategy for the
-- tiled layer when using XMonad.Layout.Spacing.
sideNavigation :: Navigation2D
sideNavigation = N 1 (doSideNavigationWithBias 1)

-- | Side navigation with bias. Consider a case where the screen is divided
-- up into three vertical panes; the side panes occupied by one window each and
-- the central pane split across the middle by two windows. By the criteria
-- of side navigation, the two central windows are equally good choices when
-- navigating inwards from one of the side panes. Hence in order to be
-- equitable, symmetric and pleasant to use, different windows are chosen when
-- navigating from different sides. In particular, the lower is chosen when
-- going left and the higher when going right, causing L, L, R, R, L, L, etc to
-- cycle through the four windows clockwise. This is implemented by using a bias
-- of 1. /Bias/ is how many pixels off centre the vertical split can be before
-- this behaviour is lost and the same window chosen every time. A negative bias
-- swaps the preferred window for each direction. A bias of zero disables the
-- behaviour.
sideNavigationWithBias :: Int -> Navigation2D
sideNavigationWithBias b = N 1 (doSideNavigationWithBias b)

-- | Hybrid of two modes of navigation, preferring the motions of the first.
-- Use this if you want to fall back on a second strategy whenever the first
-- does not find a candidate window. E.g.
-- @hybridOf lineNavigation centerNavigation@ is a good strategy for the
-- floating layer, and @hybridOf sideNavigation centerNavigation@ will enable
-- you to take advantage of some of the latter strategy's more interesting
-- motions in the tiled layer.
hybridOf :: Navigation2D -> Navigation2D -> Navigation2D
hybridOf (N g1 s1) (N g2 s2) = N (max g1 g2) $ applyToBoth s1 s2
  where
    applyToBoth f g a b c = f a b c <|> g a b c

{-# DEPRECATED hybridNavigation "Use hybridOf with lineNavigation and centerNavigation as arguments." #-}
hybridNavigation :: Navigation2D
hybridNavigation = hybridOf lineNavigation centerNavigation

-- | Stores the configuration of directional navigation. The 'Default' instance
-- uses line navigation for the tiled layer and for navigation between screens,
-- and center navigation for the float layer.  No custom navigation strategies
-- or rectangles for unmapped windows are defined for individual layouts.
data Navigation2DConfig = Navigation2DConfig
  { defaultTiledNavigation :: Navigation2D             -- ^ default navigation strategy for the tiled layer
  , floatNavigation        :: Navigation2D             -- ^ navigation strategy for the float layer
  , screenNavigation       :: Navigation2D             -- ^ strategy for navigation between screens
  , layoutNavigation       :: [(String, Navigation2D)] -- ^ association list of customized navigation strategies
                                                       -- for different layouts in the tiled layer.  Each pair
                                                       -- is of the form (\"layout description\", navigation
                                                       -- strategy).  If there is no pair in this list whose first
                                                       -- component is the name of the current layout, the
                                                       -- 'defaultTiledNavigation' strategy is used.
  , unmappedWindowRect     :: [(String, Screen -> Window -> X (Maybe Rectangle))]
                                                       -- ^ list associating functions to calculate rectangles
                                                       -- for unmapped windows with layouts to which they are
                                                       -- to be applied.  Each pair in this list is of
                                                       -- the form (\"layout description\", function), where the
                                                       -- function calculates a rectangle for a given unmapped
                                                       -- window from the screen it is on and its window ID.
                                                       -- See <#Finer_Points> for how to use this.
  }

-- | Shorthand for the tedious screen type
type Screen = WindowScreen

-- | Convenience function for enabling Navigation2D with typical keybindings.
-- Takes a Navigation2DConfig, an (up, left, down, right) tuple, a mapping from
-- modifier key to action, and a bool to indicate if wrapping should occur, and
-- returns a function from XConfig to XConfig.
-- Example:
--
-- >  navigation2D def (xK_w, xK_a, xK_s, xK_d) [(mod4Mask, windowGo), (mod4Mask .|. shiftMask, windowSwap)] False myConfig
navigation2D :: Navigation2DConfig -> (KeySym, KeySym, KeySym, KeySym) -> [(ButtonMask, Direction2D -> Bool -> X ())] ->
                Bool -> XConfig l -> XConfig l
navigation2D navConfig (u, l, d, r) modifiers wrap xconfig =
  additionalNav2DKeys (u, l, d, r) modifiers wrap $
  withNavigation2DConfig navConfig xconfig

-- | Convenience function for enabling Navigation2D with typical keybindings,
-- using the syntax defined in 'XMonad.Util.EZConfig.mkKeymap'. Takes a
-- Navigation2DConfig, an (up, left, down, right) tuple, a mapping from key
-- prefix to action, and a bool to indicate if wrapping should occur, and
-- returns a function from XConfig to XConfig. Example:
--
-- >  navigation2DP def ("w", "a", "s", "d") [("M-", windowGo), ("M-S-", windowSwap)] False myConfig
navigation2DP :: Navigation2DConfig -> (String, String, String, String) -> [(String, Direction2D -> Bool -> X ())] ->
                 Bool -> XConfig l -> XConfig l
navigation2DP navConfig (u, l, d, r) modifiers wrap xconfig =
  additionalNav2DKeysP (u, l, d, r) modifiers wrap $
  withNavigation2DConfig navConfig xconfig

-- | Convenience function for adding keybindings. Takes an (up, left, down,
-- right) tuple, a mapping from key prefix to action, and a bool to indicate if
-- wrapping should occur, and returns a function from XConfig to XConfig.
-- Example:
--
-- >  additionalNav2DKeys (xK_w, xK_a, xK_s, xK_d) [(mod4Mask, windowGo), (mod4Mask .|. shiftMask, windowSwap)] False myConfig
additionalNav2DKeys :: (KeySym, KeySym, KeySym, KeySym) -> [(ButtonMask, Direction2D -> Bool -> X ())] ->
                       Bool -> XConfig l -> XConfig l
additionalNav2DKeys (u, l, d, r) modifiers wrap =
  flip additionalKeys [((modif, k), func dir wrap) | (modif, func) <- modifiers, (k, dir) <- dirKeys]
  where dirKeys = [(u, U), (l, L), (d, D), (r, R)]

-- | Convenience function for adding keybindings, using the syntax defined in
-- 'XMonad.Util.EZConfig.mkKeymap'. Takes an (up, left, down, right) tuple, a
-- mapping from key prefix to action, and a bool to indicate if wrapping should
-- occur, and returns a function from XConfig to XConfig. Example:
--
-- >  additionalNav2DKeysP def ("w", "a", "s", "d") [("M-", windowGo), ("M-S-", windowSwap)] False myConfig
additionalNav2DKeysP :: (String, String, String, String) -> [(String, Direction2D -> Bool -> X ())] ->
                        Bool -> XConfig l -> XConfig l
additionalNav2DKeysP (u, l, d, r) modifiers wrap =
  flip additionalKeysP [(modif ++ k, func dir wrap) | (modif, func) <- modifiers, (k, dir) <- dirKeys]
  where dirKeys = [(u, U), (l, L), (d, D), (r, R)]

-- So we can store the configuration in extensible state
instance ExtensionClass Navigation2DConfig where
  initialValue = def

-- | Modifies the xmonad configuration to store the Navigation2D configuration
withNavigation2DConfig :: Navigation2DConfig -> XConfig a -> XConfig a
withNavigation2DConfig conf2d xconf = xconf { startupHook  = startupHook xconf
                                                          >> XS.put conf2d
                                            }

instance Default Navigation2DConfig where
    def                   = Navigation2DConfig { defaultTiledNavigation = lineNavigation
                                               , floatNavigation        = centerNavigation
                                               , screenNavigation       = lineNavigation
                                               , layoutNavigation       = []
                                               , unmappedWindowRect     = []
                                               }

-- | Switches focus to the closest window in the other layer (floating if the
-- current window is tiled, tiled if the current window is floating).  Closest
-- means that the L1-distance between the centers of the windows is minimized.
switchLayer :: X ()
switchLayer = actOnLayer otherLayer
                         ( \ _ cur wins -> windows
                           $ doFocusClosestWindow cur wins
                         )
                         ( \ _ cur wins -> windows
                           $ doFocusClosestWindow cur wins
                         )
                         ( \ _ _ _ -> return () )
                         False

-- | Moves the focus to the next window in the given direction and in the same
-- layer as the current window.  The second argument indicates whether
-- navigation should wrap around (e.g., from the left edge of the leftmost
-- screen to the right edge of the rightmost screen).
windowGo :: Direction2D -> Bool -> X ()
windowGo dir = actOnLayer thisLayer
                               ( \ conf cur wins -> windows
                                 $ doTiledNavigation conf dir W.focusWindow cur wins
                               )
                               ( \ conf cur wins -> windows
                                 $ doFloatNavigation conf dir W.focusWindow cur wins
                               )
                               ( \ conf cur wspcs -> windows
                                 $ doScreenNavigation conf dir W.view cur wspcs
                               )

-- | Swaps the current window with the next window in the given direction and in
-- the same layer as the current window.  (In the floating layer, all that
-- changes for the two windows is their stacking order if they're on the same
-- screen.  If they're on different screens, each window is moved to the other
-- window's screen but retains its position and size relative to the screen.)
-- The second argument indicates wrapping (see 'windowGo').
windowSwap :: Direction2D -> Bool -> X ()
windowSwap dir = actOnLayer thisLayer
                                 ( \ conf cur wins -> windows
                                   $ doTiledNavigation conf dir swap cur wins
                                 )
                                 ( \ conf cur wins -> windows
                                   $ doFloatNavigation conf dir swap cur wins
                                 )
                                 ( \ _ _ _ -> return () )

-- | Moves the current window to the next screen in the given direction.  The
-- second argument indicates wrapping (see 'windowGo').
windowToScreen :: Direction2D -> Bool -> X ()
windowToScreen dir = actOnScreens ( \ conf cur wspcs -> windows
                                         $ doScreenNavigation conf dir W.shift cur wspcs
                                       )

-- | Moves the focus to the next screen in the given direction.  The second
-- argument indicates wrapping (see 'windowGo').
screenGo :: Direction2D -> Bool -> X ()
screenGo dir = actOnScreens ( \ conf cur wspcs -> windows
                                   $ doScreenNavigation conf dir W.view cur wspcs
                                 )

-- | Swaps the workspace on the current screen with the workspace on the screen
-- in the given direction.  The second argument indicates wrapping (see
-- 'windowGo').
screenSwap :: Direction2D -> Bool -> X ()
screenSwap dir = actOnScreens ( \ conf cur wspcs -> windows
                                     $ doScreenNavigation conf dir W.greedyView cur wspcs
                                   )

-- | Maps each window to a fullscreen rect.  This may not be the same rectangle the
-- window maps to under the Full layout or a similar layout if the layout
-- respects statusbar struts.  In such cases, it may be better to use
-- 'singleWindowRect'.
fullScreenRect :: Screen -> Window -> X (Maybe Rectangle)
fullScreenRect scr _ = return (Just . screenRect . W.screenDetail $ scr)

-- | Maps each window to the rectangle it would receive if it was the only
-- window in the layout.  Useful, for example, for determining the default
-- rectangle for unmapped windows in a Full layout that respects statusbar
-- struts.
singleWindowRect :: Screen -> Window -> X (Maybe Rectangle)
singleWindowRect scr win  =  listToMaybe
                          .  map snd
                          .  fst
                         <$> runLayout ((W.workspace scr) { W.stack = W.differentiate [win] })
                                       (screenRect . W.screenDetail $ scr)

----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
--                                                                                                --
--                                       PRIVATE X ACTIONS                                        --
--                                                                                                --
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

-- | Acts on the appropriate layer using the given action functions
actOnLayer :: ([WinRect] -> [WinRect] -> [WinRect])                -- ^ Chooses which layer to operate on, relative
                                                                   -- to the current window (same or other layer)
           -> (Navigation2DConfig -> WinRect -> [WinRect] -> X ()) -- ^ The action for the tiled layer
           -> (Navigation2DConfig -> WinRect -> [WinRect] -> X ()) -- ^ The action for the float layer
           -> (Navigation2DConfig -> WSRect  -> [WSRect]  -> X ()) -- ^ The action if the current workspace is empty
           -> Bool                                                 -- ^ Should navigation wrap around screen edges?
           -> X ()
actOnLayer choice tiledact floatact wsact wrap = withWindowSet $ \winset -> do
  conf <- XS.get
  (floating, tiled) <- navigableWindows conf wrap winset
  let cur = W.peek winset
  case cur of
    Nothing                                   -> actOnScreens wsact wrap
    Just w | Just rect <- L.lookup w tiled    -> tiledact conf (w, rect) (choice tiled floating)
           | Just rect <- L.lookup w floating -> floatact conf (w, rect) (choice floating tiled)
           | otherwise                        -> return ()

-- | Returns the list of windows on the currently visible workspaces
navigableWindows :: Navigation2DConfig -> Bool -> WindowSet -> X ([WinRect], [WinRect])
navigableWindows conf wrap winset  =  L.partition (\(win, _) -> M.member win (W.floating winset))
                                   .  addWrapping winset wrap
                                   .  catMaybes
                                   .  concat
                                  <$>
                                   (  mapM ( \scr -> mapM (maybeWinRect scr)
                                                   $ W.integrate'
                                                   $ W.stack
                                                   $ W.workspace scr
                                           )
                                   .  sortedScreens
                                   )  winset
  where
    maybeWinRect scr win = do
      winrect <- windowRect win
      rect <- case winrect of
                Just _  -> return winrect
                Nothing -> maybe (return Nothing)
                                 (\f -> f scr win)
                                 (L.lookup (description . W.layout . W.workspace $ scr) (unmappedWindowRect conf))
      return ((,) win <$> rect)

-- | Returns the current rectangle of the given window, Nothing if the window isn't mapped
windowRect :: Window -> X (Maybe Rectangle)
windowRect win = withDisplay $ \dpy -> do
  mp <- isMapped win
  if mp then do (_, x, y, w, h, bw, _) <- io $ getGeometry dpy win
                return $ Just $ Rectangle x y (w + 2 * bw) (h + 2 * bw)
                `catchX` return Nothing
        else return Nothing

-- | Acts on the screens using the given action function
actOnScreens :: (Navigation2DConfig -> WSRect -> [WSRect] -> X ())
             -> Bool  -- ^ Should wrapping be used?
             -> X ()
actOnScreens act wrap = withWindowSet $ \winset -> do
  conf <- XS.get
  let wsrects = visibleWorkspaces winset wrap
      cur     = W.tag . W.workspace . W.current $ winset
      rect    = fromJust $ L.lookup cur wsrects
  act conf (cur, rect) wsrects

-- | Determines whether a given window is mapped
isMapped :: Window -> X Bool
isMapped = fmap (maybe False ((waIsUnmapped /=) .  wa_map_state))
         . safeGetWindowAttributes

----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
--                                                                                                --
--                                     PRIVATE PURE FUNCTIONS                                     --
--                                                                                                --
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

-- | Finds the window closest to the given window and focuses it. Ties are
-- broken by choosing the first window in the window stack among the tied
-- windows.  (The stack order is the one produced by integrate'ing each visible
-- workspace's window stack and concatenating these lists for all visible
-- workspaces.)
doFocusClosestWindow :: WinRect
                     -> [WinRect]
                     -> (WindowSet -> WindowSet)
doFocusClosestWindow (cur, rect) winrects
  | null winctrs = id
  | otherwise    = W.focusWindow . fst $ L.foldl1' closer winctrs
  where
    ctr     = centerOf rect
    winctrs = filter ((cur /=) . fst)
            $ map (second centerOf) winrects
    closer wc1@(_, c1) wc2@(_, c2) | lDist ctr c1 > lDist ctr c2 = wc2
                                   | otherwise                   = wc1

-- | Implements navigation for the tiled layer
doTiledNavigation :: Navigation2DConfig
                  -> Direction2D
                  -> (Window -> WindowSet -> WindowSet)
                  -> WinRect
                  -> [WinRect]
                  -> (WindowSet -> WindowSet)
doTiledNavigation conf dir act cur winrects winset
  | Just win <- runNav nav dir cur winrects = act win winset
  | otherwise                               = winset
  where
    layouts = map (description . W.layout . W.workspace)
            $ W.screens winset
    nav     = maximum
            $ map ( fromMaybe (defaultTiledNavigation conf)
                  . flip L.lookup (layoutNavigation conf)
                  ) layouts

-- | Implements navigation for the float layer
doFloatNavigation :: Navigation2DConfig
                  -> Direction2D
                  -> (Window -> WindowSet -> WindowSet)
                  -> WinRect
                  -> [WinRect]
                  -> (WindowSet -> WindowSet)
doFloatNavigation conf dir act cur winrects
  | Just win <- runNav nav dir cur winrects = act win
  | otherwise                               = id
  where
    nav = floatNavigation conf

-- | Implements navigation between screens
doScreenNavigation :: Navigation2DConfig
                   -> Direction2D
                   -> (WorkspaceId -> WindowSet -> WindowSet)
                   -> WSRect
                   -> [WSRect]
                   -> (WindowSet -> WindowSet)
doScreenNavigation conf dir act cur wsrects
  | Just ws <- runNav nav dir cur wsrects = act ws
  | otherwise                             = id
  where
    nav = screenNavigation conf

-- | Implements line navigation.  For layouts without overlapping windows, there
-- is no need to break ties between equidistant windows.  When windows do
-- overlap, even the best tie breaking rule cannot make line navigation feel
-- natural.  Thus, we fairly arbtitrarily break ties by preferring the window
-- that comes first in the window stack.  (The stack order is the one produced
-- by integrate'ing each visible workspace's window stack and concatenating
-- these lists for all visible workspaces.)
doLineNavigation :: Eq a => Direction2D -> Rect a -> [Rect a] -> Maybe a
doLineNavigation dir (cur, rect) winrects
  | null winrects' = Nothing
  | otherwise      = Just . fst $ L.foldl1' closer winrects'
  where
    -- The current window's center
    ctr@(xc, yc)  = centerOf rect

    -- The list of windows that are candidates to receive focus.
    winrects'     = filter dirFilter
                  . filter ((cur /=) . fst)
                  $ winrects

    -- Decides whether a given window matches the criteria to be a candidate to
    -- receive the focus.
    dirFilter (_, r) =  (dir == L && leftOf r rect && intersectsY yc r)
                     || (dir == R && leftOf rect r && intersectsY yc r)
                     || (dir == U && above  r rect && intersectsX xc r)
                     || (dir == D && above  rect r && intersectsX xc r)

    -- Decide whether r1 is left of/above r2.
    leftOf r1 r2 = rect_x r1 + fi (rect_width  r1) <= rect_x r2
    above  r1 r2 = rect_y r1 + fi (rect_height r1) <= rect_y r2

    -- Check whether r's x-/y-range contains the given x-/y-coordinate.
    intersectsX x r = rect_x r <= x && rect_x r + fi (rect_width  r) >= x
    intersectsY y r = rect_y r <= y && rect_y r + fi (rect_height r) >= y

    -- Decides whether r1 is closer to the current window's center than r2
    closer wr1@(_, r1) wr2@(_, r2) | dist ctr r1 > dist ctr r2 = wr2
                                   | otherwise                 = wr1

    -- Returns the distance of r from the point (x, y)
    dist (x, y) r | dir == L  = x - rect_x r - fi (rect_width r)
                  | dir == R  = rect_x r - x
                  | dir == U  = y - rect_y r - fi (rect_height r)
                  | otherwise = rect_y r - y

-- | Implements center navigation
doCenterNavigation :: Eq a => Direction2D -> Rect a -> [Rect a] -> Maybe a
doCenterNavigation dir (cur, rect) winrects
  | ((w, _):_) <- onCtr' = Just w
  | otherwise            = closestOffCtr
  where
    -- The center of the current window
    (xc, yc) = centerOf rect

    -- All the windows with their center points relative to the current
    -- center rotated so the right cone becomes the relevant cone.
    -- The windows are ordered in the order they should be preferred
    -- when they are otherwise tied.
    winctrs = map (second (dirTransform . centerOf))
            $ stackTransform winrects

    -- Give preference to windows later in the stack for going left or up and to
    -- windows earlier in the stack for going right or down.  (The stack order
    -- is the one produced by integrate'ing each visible workspace's window
    -- stack and concatenating these lists for all visible workspaces.)
    stackTransform | dir == L || dir == U = reverse
                   | otherwise            = id

    -- Transform a point into a difference to the current window center and
    -- rotate it so that the relevant cone becomes the right cone.
    dirTransform (x, y) | dir == R  = (  x - xc ,   y - yc )
                        | dir == L  = (-(x - xc), -(y - yc))
                        | dir == D  = (  y - yc ,   x - xc )
                        | otherwise = (-(y - yc), -(x - xc))

    -- Partition the points into points that coincide with the center
    -- and points that do not.
    (onCtr, offCtr) = L.partition (\(_, (x, y)) -> x == 0 && y == 0) winctrs

    -- All the points that coincide with the current center and succeed it
    -- in the (appropriately ordered) window stack.
    onCtr' = L.tail $ L.dropWhile ((cur /=) . fst) onCtr
             -- tail should be safe here because cur should be in onCtr

    -- All the points that do not coincide with the current center and which
    -- lie in the (rotated) right cone.
    offCtr' = L.filter (\(_, (x, y)) -> x > 0 && y < x && y >= -x) offCtr

    -- The off-center point closest to the center and
    -- closest to the bottom ray of the cone.  Nothing if no off-center
    -- point is in the cone
    closestOffCtr = if null offCtr' then Nothing
                                    else Just $ fst $ L.foldl1' closest offCtr'

    closest wp@(_, p@(_, yp)) wq@(_, q@(_, yq))
      | lDist (0, 0) q < lDist (0, 0) p = wq -- q is closer than p
      | lDist (0, 0) p < lDist (0, 0) q = wp -- q is farther away than p
      | yq < yp                         = wq -- q is closer to the bottom ray than p
      | otherwise                       = wp -- q is farther away from the bottom ray than p
                                             -- or it has the same distance but comes later
                                             -- in the window stack

-- x -y w h format is a pain. Let's use side coordinates. We assume x1 <= x2 and
-- y1 <= y2, and make the assumption valid by initialising SideRects with the
-- property and carefully preserving it over any individual transformation.
data SideRect = SideRect { x1 :: Int, x2 :: Int, y1 :: Int, y2 :: Int }
  deriving Show

-- Conversion from Rectangle format to SideRect.
toSR :: Rectangle -> SideRect
toSR (Rectangle x y w h) = SideRect (fi x) (fi x + fi w) (-fi y - fi h) (-fi y)

-- Implements side navigation with bias.
doSideNavigationWithBias ::
  Eq a => Int -> Direction2D -> Rect a -> [Rect a] -> Maybe a
doSideNavigationWithBias bias dir (cur, rect)
  = fmap fst . listToMaybe
  . L.sortOn dist . foldr acClosest []
  . filter (`toRightOf` (cur, transform rect))
  . map (fmap transform)
  where
    -- Getting the center of the current window so we can make it the new origin.
    cOf r = ((x1 r + x2 r) `div` 2, (y1 r + y2 r) `div` 2)
    (x0, y0) = cOf . toSR $ rect

    -- Translate the given SideRect by (-x0, -y0).
    translate r = SideRect (x1 r - x0) (x2 r - x0) (y1 r - y0) (y2 r - y0)

    -- Rotate the given SideRect 90 degrees counter-clockwise about the origin.
    rHalfPiCC r = SideRect (-y2 r) (-y1 r) (x1 r) (x2 r)

    -- Apply the above function until d becomes synonymous with R (wolog).
    rotateToR d = fromJust . lookup d . zip [R, D, L, U] . iterate rHalfPiCC

    transform = rotateToR dir . translate . toSR

    -- (_, r) `toRightOf` (_, c) iff r has points to the right of c that aren't
    -- below or above c, i.e. iff:
    -- [x1 r, x2 r] x [y1 r, y2 r] intersects (x2 c, infinity) x (y1 c, y2 c)
    toRightOf (_, r) (_, c) = (x2 r > x2 c) && (y2 r > y1 c) && (y1 r < y2 c)

    -- Greedily accumulate the windows tied for the leftmost left side.
    acClosest (w, r) l@((_, r'):_) | x1 r == x1 r' = (w, r) : l
                                   | x1 r >  x1 r' =          l
    acClosest (w, r) _                             = [(w, r)]

    -- Given a (_, SideRect), calculate how far it is from the y=bias line.
    dist (_, r) | (y1 r <= bias) && (bias <= y2 r) = 0
                | otherwise = min (abs $ y1 r - bias) (abs $ y2 r - bias)

-- | Swaps the current window with the window given as argument
swap :: Window -> WindowSet -> WindowSet
swap win winset = W.focusWindow cur
                $ L.foldl' (flip W.focusWindow) newwinset newfocused
  where
    -- The current window
    cur      = fromJust $ W.peek winset

    -- All screens
    scrs     = W.screens winset

    -- All visible workspaces
    visws    = map W.workspace scrs

    -- The focused windows of the visible workspaces
    focused  = mapMaybe (fmap W.focus . W.stack) visws

    -- The window lists of the visible workspaces
    wins     = map (W.integrate' . W.stack) visws

    -- Update focused windows and window lists to reflect swap of windows.
    newfocused = map swapWins focused
    newwins    = map (map swapWins) wins

    -- Replaces the current window with the argument window and vice versa.
    swapWins x | x == cur  = win
               | x == win  = cur
               | otherwise = x

    -- Reconstruct the workspaces' window stacks to reflect the swap.
    newvisws  = zipWith (\ws wns -> ws { W.stack = W.differentiate wns }) visws newwins
    newscrs   = zipWith (\scr ws -> scr { W.workspace = ws }) scrs newvisws
    newwinset = winset { W.current = head newscrs
                       , W.visible = tail newscrs
                       }

-- | Calculates the center of a rectangle
centerOf :: Rectangle -> (Position, Position)
centerOf r = (rect_x r + fi (rect_width r) `div` 2, rect_y r + fi (rect_height r) `div` 2)

-- | Functions to choose the subset of windows to operate on
thisLayer, otherLayer :: a -> a -> a
thisLayer  = const
otherLayer _ x = x

-- | Returns the list of visible workspaces and their screen rects
visibleWorkspaces :: WindowSet -> Bool -> [WSRect]
visibleWorkspaces winset wrap = addWrapping winset wrap
                              $ map ( \scr -> ( W.tag . W.workspace         $ scr
                                              , screenRect . W.screenDetail $ scr
                                              )
                                    )
                              $ sortedScreens winset

-- | Creates five copies of each (window/workspace, rect) pair in the input: the
-- original and four offset one desktop size (desktop = collection of all
-- screens) to the left, to the right, up, and down.  Wrap-around at desktop
-- edges is implemented by navigating into these displaced copies.
addWrapping :: WindowSet -- ^ The window set, used to get the desktop size
            -> Bool      -- ^ Should wrapping be used?  Do nothing if not.
            -> [Rect a]  -- ^ Input set of (window/workspace, rect) pairs
            -> [Rect a]
addWrapping _      False wrects = wrects
addWrapping winset True  wrects = [ (w, r { rect_x = rect_x r + fi x
                                          , rect_y = rect_y r + fi y
                                          }
                                    )
                                  | (w, r) <- wrects
                                  , (x, y)  <- [(0, 0), (-xoff, 0), (xoff, 0), (0, -yoff), (0, yoff)]
                                  ]
  where
    (xoff, yoff) = wrapOffsets winset

-- | Calculates the offsets for window/screen coordinates for the duplication
-- of windows/workspaces that implements wrap-around.
wrapOffsets :: WindowSet -> (Integer, Integer)
wrapOffsets winset = (max_x - min_x, max_y - min_y)
  where
    min_x = fi $ minimum $ map rect_x rects
    min_y = fi $ minimum $ map rect_y rects
    max_x = fi $ maximum $ map (\r -> rect_x r + fi (rect_width  r)) rects
    max_y = fi $ maximum $ map (\r -> rect_y r + fi (rect_height r)) rects
    rects = map snd $ visibleWorkspaces winset False


-- | Returns the list of screens sorted primarily by their centers'
-- x-coordinates and secondarily by their y-coordinates.
sortedScreens :: WindowSet -> [Screen]
sortedScreens winset = L.sortBy cmp
                     $ W.screens winset
  where
    cmp s1 s2 | x < x'   = LT
              | x > x'   = GT
              | y < x'   = LT
              | y > y'   = GT
              | otherwise = EQ
      where
        (x , y ) = centerOf (screenRect . W.screenDetail $ s1)
        (x', y') = centerOf (screenRect . W.screenDetail $ s2)


-- | Calculates the L1-distance between two points.
lDist :: (Position, Position) -> (Position, Position) -> Int
lDist (x, y) (x', y') = abs (fi $ x - x') + abs (fi $ y - y')
