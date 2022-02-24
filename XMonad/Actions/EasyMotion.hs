{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Actions.EasyMotion
-- Description :  Focus a visible window using a key chord.
-- Copyright   :  (c) Matt Kingston <mattkingston@gmail.com>
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  mattkingston@gmail.com
-- Stability   :  unstable
-- Portability :  unportable
--
-- Provides functionality to use key chords to focus a visible window. Overlays a unique key chord
-- (a string) above each visible window and allows the user to select a window by typing that
-- chord.
-- Inspired by <https://github.com/easymotion/vim-easymotion vim-easymotion>.
-- Thanks to <https://github.com/larkery Tom Hinton> for some feature inspiration and window
-- sorting code.
--
-----------------------------------------------------------------------------

module XMonad.Actions.EasyMotion ( -- * Usage
                                   -- $usage
                                   selectWindow

                                   -- * Configuration
                                 , EasyMotionConfig(..)
                                 , ChordKeys(..)
                                 , def

                                   -- * Creating overlays
                                 , fullSize
                                 , fixedSize
                                 , textSize
                                 , proportional
                                 , bar
                                 ) where

import           XMonad
import           XMonad.Prelude
import qualified XMonad.StackSet          as W
import           XMonad.Util.Font         (releaseXMF, initXMF, Align(AlignCenter), XMonadFont(..), textExtentsXMF)
import           XMonad.Util.XUtils       (createNewWindow, paintAndWrite, deleteWindow, showWindow)

import           Control.Arrow            ((&&&))
import qualified Data.Map.Strict as M     (Map, elems, map, mapWithKey)

-- $usage
--
-- You can use this module's basic functionality with the following in your
-- @~\/.xmonad\/xmonad.hs@:
--
-- >    import XMonad.Actions.EasyMotion (selectWindow)
--
-- To customise
--
-- >    import XMonad.Actions.EasyMotion (selectWindow, EasyMotionConfig(..))
--
-- Then add a keybinding and an action to the 'selectWindow' function.
-- In this case @M-f@ to focus the selected window:
--
-- >    , ((modm, xK_f), selectWindow def >>= (`whenJust` windows . W.focusWindow))
--
-- Similarly, to kill a window with @M-f@:
--
-- >    , ((modm, xK_f), selectWindow def >>= (`whenJust` killWindow))
--
-- See 'EasyMotionConfig' for all configuration options. A short summary follows.
--
-- Default chord keys are @s,d,f,j,k,l@. To customise these and display options assign
-- different values to 'def' (the default configuration):
--
-- >    , ((modm, xK_f), (selectWindow def{sKeys = AnyKeys [xK_f, xK_d]}) >>= (`whenJust` windows . W.focusWindow))
--
-- You must supply at least two different keys in the 'sKeys' list. Keys provided earlier in the list
-- will be used preferentially—therefore, keys you would like to use more frequently should be
-- earlier in the list.
--
-- To map different sets of keys to different screens. The following configuration maps keys @fdsa@
-- to screen 0 and @hjkl@ to screen 1. Keys provided earlier in the list will be used preferentially.
-- Providing the same key for multiple screens is possible but will break down in some scenarios.
--
-- >    import qualified Data.Map.Strict as StrictMap (fromList)
-- >    emConf :: EasyMotionConfig
-- >    emConf = def { sKeys = PerScreenKeys $ StrictMap.fromList [(0, [xK_f, xK_d, xK_s, xK_a]), (1, [xK_h, xK_j, xK_k, xK_l])] }
-- >    -- key bindings
-- >    , ((modm, xK_f), selectWindow emConf >>= (`whenJust` windows . W.focusWindow))
--
-- To customise the font:
--
-- >    , ((modm, xK_f), (selectWindow def{emFont = "xft: Sans-40"}) >>= (`whenJust` windows . W.focusWindow))
--
-- The 'emFont' field provided is supplied directly to the 'initXMF' function. The default is
-- @"xft:Sans-100"@. Some example options:
--
-- >    "xft: Sans-40"
-- >    "xft: Arial-100"
-- >    "xft: Cambria-80"
--
-- Customise the overlay by supplying a function to 'overlayF'. The signature is
-- @'Position' -> 'Rectangle' -> 'Rectangle'@. The parameters are the height in pixels of
-- the selection chord and the rectangle of the window to be overlaid. Some are provided:
--
-- >    import XMonad.Actions.EasyMotion (selectWindow, EasyMotionConfig(..), proportional, bar, fullSize)
-- >    , ((modm, xK_f), (selectWindow def{ overlayF = proportional 0.3  }) >>= (`whenJust` windows . W.focusWindow))
-- >    , ((modm, xK_f), (selectWindow def{ overlayF = bar 0.5           }) >>= (`whenJust` windows . W.focusWindow))
-- >    , ((modm, xK_f), (selectWindow def{ overlayF = fullSize          }) >>= (`whenJust` windows . W.focusWindow))
-- >    , ((modm, xK_f), (selectWindow def{ overlayF = fixedSize 300 350 }) >>= (`whenJust` windows . W.focusWindow))

-- TODO:
--  - An overlay function that creates an overlay a proportion of the width XOR height of the
--    window it's over, and with a fixed w/h proportion? E.g. overlay-height = 0.3 *
--    target-window-height; overlay-width = 0.5 * overlay-height.
--  - An overlay function that creates an overlay of a fixed w,h, aligned mid,mid, or parametrised
--    alignment?
--  - Parametrise chord generation?
--  - W.shift example; bring window from other screen to current screen? Only useful if we don't
--    show chords on current workspace.
--  - Use stringToKeysym, keysymToKeycode, keycodeToKeysym, keysymToString to take a string from
--    the user?
--  - Think a bit more about improving functionality with floating windows.
--    - currently, floating window z-order is not respected
--    - could ignore floating windows
--    - may be able to calculate the visible section of a floating window, and display the chord in
--      that space
--  - Provide an option to prepend the screen key to the easymotion keys (i.e. w,e,r)?
--  - overlay alpha
--  - Delay after selection so the user can see what they've chosen? Min-delay: 0 seconds. If
--    there's a delay, perhaps keep the other windows covered briefly to naturally draw the user's
--    attention to the window they've selected? Or briefly highlight the border of the selected
--    window?
--  - Option to cover windows that will not be selected by the current chord, such that it's
--    slightly more obvious where to maintain focus.
--  - Something unpleasant happens when the user provides only two keys (let's say f, d) for
--    chords. When they have five windows open, the following chords are generated: ddd, ddf, dfd,
--    dff, fdd. When 'f' is pressed, all chords disappear unexpectedly because we know there are no
--    other valid options. The user expects to press 'fdd'. This is an optimisation in software but
--    pretty bad for usability, as the user continues firing keys into their
--    now-unexpectedly-active window. And is of course only one concrete example of a more general
--    problem.
--    Short-term solution:
--      - Keep displaying the chord until the user has fully entered it
--    Fix:
--      - Show the shortest possible chords

-- | Associates a user window, an overlay window created by this module and a rectangle
--   circumscribing these windows
data OverlayWindow =
  OverlayWindow { win     :: !Window           -- ^ The window managed by xmonad
                , attrs   :: !WindowAttributes -- ^ Window attributes for @win@
                , overlay :: !Window           -- ^ Our window used to display the overlay
                , rect    :: !Rectangle        -- ^ The rectangle of @overlay@
                }

-- | An overlay window and the chord used to select it
data Overlay =
  Overlay { overlayWin :: !OverlayWindow    -- ^ The window managed by xmonad
          , chord      :: ![KeySym]         -- ^ The chord we'll display in the overlay
          }


-- | Maps keys to windows. 'AnyKeys' maps keys to windows regardless which screen they're on.
--   'PerScreenKeys' maps keys to screens to windows. See @Usage@ for more examples.
data ChordKeys = AnyKeys       ![KeySym]
               | PerScreenKeys !(M.Map ScreenId [KeySym])

-- | Configuration options for EasyMotion.
--
--   All colors are hex strings, e.g. "#000000"
--
--   If the number of windows for which chords are required exceeds 'maxChordLen', chords
--   will simply not be generated for these windows. In this way, single-key selection may be
--   preferred over the ability to select any window.
--
--   'cancelKey', @xK_BackSpace@ and any duplicates will be removed from 'sKeys' if included.
--   See @Usage@ for examples of 'sKeys'.
data EasyMotionConfig =
  EMConf { txtCol      :: !String                               -- ^ Color of the text displayed
         , bgCol       :: !String                               -- ^ Color of the window overlaid
         , overlayF    :: !(Position -> Rectangle -> Rectangle) -- ^ Function to generate overlay rectangle
         , borderCol   :: !String                               -- ^ Color of the overlay window borders
         , sKeys       :: !ChordKeys                            -- ^ Keys to use for window selection
         , cancelKey   :: !KeySym                               -- ^ Key to use to cancel selection
         , emFont      :: !String                               -- ^ Font for selection characters (passed to 'initXMF')
         , borderPx    :: !Int                                  -- ^ Width of border in pixels
         , maxChordLen :: !Int                                  -- ^ Maximum chord length. Use 0 for no maximum.
         }

instance Default EasyMotionConfig where
  def =
    EMConf { txtCol      = "#ffffff"
           , bgCol       = "#000000"
           , overlayF    = proportional (0.3::Double)
           , borderCol   = "#ffffff"
           , sKeys       = AnyKeys [xK_s, xK_d, xK_f, xK_j, xK_k, xK_l]
           , cancelKey   = xK_q
           , borderPx    = 1
           , maxChordLen = 0
#ifdef XFT
           , emFont      = "xft:Sans-100"
#else
           , emFont      = "-misc-fixed-*-*-*-*-200-*-*-*-*-*-*-*"
#endif
           }

-- | Create overlay windows of the same size as the window they select
fullSize :: Position -> Rectangle -> Rectangle
fullSize _ = id

-- | Create overlay windows a proportion of the size of the window they select
proportional :: RealFrac f => f -> Position -> Rectangle -> Rectangle
proportional f th r = Rectangle { rect_width  = newW
                                , rect_height = newH
                                , rect_x      = rect_x r + fi (rect_width r - newW) `div` 2
                                , rect_y      = rect_y r + fi (rect_height r - newH) `div` 2 }
 where
  newH = max (fi th) (round $ f * fi (rect_height r))
  newW = newH

-- | Create fixed-size overlay windows
fixedSize :: (Integral a, Integral b) => a -> b -> Position -> Rectangle -> Rectangle
fixedSize w h th r = Rectangle { rect_width  = rw
                               , rect_height = rh
                               , rect_x      = rect_x r + fi (rect_width r - rw) `div` 2
                               , rect_y      = rect_y r + fi (rect_height r - rh) `div` 2 }
 where
  rw = max (fi w) (fi th)
  rh = max (fi h) (fi th)

-- | Create overlay windows the minimum size to contain their key chord
textSize :: Position -> Rectangle -> Rectangle
textSize th r = Rectangle { rect_width  = fi th
                          , rect_height = fi th
                          , rect_x      = rect_x r + (fi (rect_width r) - fi th) `div` 2
                          , rect_y      = rect_y r + (fi (rect_height r) - fi th) `div` 2 }

-- | Create overlay windows the full width of the window they select, the minimum height to contain
--   their chord, and a proportion of the distance from the top of the window they select
bar :: RealFrac f => f -> Position -> Rectangle -> Rectangle
bar f th r = Rectangle { rect_width  = rect_width r
                       , rect_height = fi th
                       , rect_x      = rect_x r
                       , rect_y      = rect_y r + round (f' * (fi (rect_height r) - fi th)) }
 where
   -- clamp f in [0,1] as other values will appear to lock up xmonad as the overlay will be
   -- displayed off-screen
   f' = min 0.0 $ max f 1.0

-- | Handles overlay display and window selection. Called after config has been sanitised.
handleSelectWindow :: EasyMotionConfig -> X (Maybe Window)
handleSelectWindow EMConf { sKeys = AnyKeys [] } = return Nothing
handleSelectWindow c = do
  f <- initXMF $ emFont c
  th <- (\(asc, dsc) -> asc + dsc + 2) <$> textExtentsXMF f (concatMap keysymToString (allKeys . sKeys $ c))
  XConf { theRoot = rw, display = dpy } <- ask
  XState { mapped = mappedWins, windowset = ws } <- get
  -- build overlays depending on key configuration
  overlays :: [Overlay] <- case sKeys c of
    AnyKeys ks -> buildOverlays ks <$> sortedOverlayWindows
     where
      visibleWindows :: [Window]
      visibleWindows = toList mappedWins
      sortedOverlayWindows :: X [OverlayWindow]
      sortedOverlayWindows = sortOverlayWindows <$> buildOverlayWindows th visibleWindows
    PerScreenKeys m ->
      fmap concat
        $ sequence
        $ M.elems
        $ M.mapWithKey (\sid ks -> buildOverlays ks <$> sortedOverlayWindows sid) m
     where
      screenById :: ScreenId -> Maybe WindowScreen
      screenById sid = find ((== sid) . W.screen) (W.screens ws)
      visibleWindowsOnScreen :: ScreenId -> [Window]
      visibleWindowsOnScreen sid = filter (`elem` toList mappedWins) $ W.integrate' $ screenById sid >>= W.stack . W.workspace
      sortedOverlayWindows :: ScreenId -> X [OverlayWindow]
      sortedOverlayWindows sid = sortOverlayWindows <$> buildOverlayWindows th (visibleWindowsOnScreen sid)
  status <- io $ grabKeyboard dpy rw True grabModeAsync grabModeAsync currentTime
  if status == grabSuccess
    then do
      resultWin <- handleKeyboard dpy (displayOverlay f) (cancelKey c) overlays []
      io $ ungrabKeyboard dpy currentTime
      mapM_ (deleteWindow . overlay . overlayWin) overlays
      io $ sync dpy False
      releaseXMF f
      case resultWin of
        -- focus the selected window
        Selected o -> return . Just . win . overlayWin $ o
        -- return focus correctly
        _ -> whenJust (W.peek ws) (windows . W.focusWindow) $> Nothing
    else releaseXMF f $> Nothing
 where
  allKeys :: ChordKeys -> [KeySym]
  allKeys (AnyKeys ks) = ks
  allKeys (PerScreenKeys m) = concat $ M.elems m

  buildOverlays :: [KeySym] -> [OverlayWindow] -> [Overlay]
  buildOverlays = appendChords (maxChordLen c)

  buildOverlayWindows :: Position -> [Window] -> X [OverlayWindow]
  buildOverlayWindows th = fmap (fromMaybe [] . sequenceA)
                         . traverse (buildOverlayWin th)

  sortOverlayWindows :: [OverlayWindow] -> [OverlayWindow]
  sortOverlayWindows = sortOn ((wa_x &&& wa_y) . attrs)

  makeRect :: WindowAttributes -> Rectangle
  makeRect wa = Rectangle (fi (wa_x wa)) (fi (wa_y wa)) (fi (wa_width wa)) (fi (wa_height wa))

  buildOverlayWin :: Position -> Window -> X (Maybe OverlayWindow)
  buildOverlayWin th w = safeGetWindowAttributes w >>= \case
    Nothing     -> pure Nothing
    Just wAttrs -> do
      let r = overlayF c th $ makeRect wAttrs
      o <- createNewWindow r Nothing "" True
      return . Just $ OverlayWindow { rect=r, overlay=o, win=w, attrs=wAttrs }

  -- | Display an overlay with the provided formatting
  displayOverlay :: XMonadFont -> Overlay -> X ()
  displayOverlay f Overlay { overlayWin = OverlayWindow { rect = r, overlay = o }, chord = ch } = do
    showWindow o
    paintAndWrite o f (fi (rect_width r)) (fi (rect_height r)) (fi (borderPx c)) (bgCol c) (borderCol c) (txtCol c) (bgCol c) [AlignCenter] [concatMap keysymToString ch]

-- | Display overlay windows and chords for window selection
selectWindow :: EasyMotionConfig -> X (Maybe Window)
selectWindow conf =
  handleSelectWindow conf { sKeys = sanitiseKeys (sKeys conf) }
 where
  -- make sure the key lists don't contain: backspace, our cancel key, or duplicates
  sanitise :: [KeySym] -> [KeySym]
  sanitise = nub . filter (`notElem` [xK_BackSpace, cancelKey conf])
  sanitiseKeys :: ChordKeys -> ChordKeys
  sanitiseKeys cKeys =
    case cKeys of
      AnyKeys ks -> AnyKeys . sanitise $ ks
      PerScreenKeys m -> PerScreenKeys $ M.map sanitise m

-- | Take a list of overlays lacking chords, return a list of overlays with key chords
appendChords :: Int -> [KeySym] -> [OverlayWindow] -> [Overlay]
appendChords _ [] _ = []
appendChords maxUserSelectedLen ks overlayWins =
  zipWith Overlay overlayWins chords
 where
  chords = replicateM chordLen ks
  -- the minimum necessary chord length to assign a unique chord to each visible window
  minCoverLen = -((-(length overlayWins)) `div` length ks)
  -- if the user has specified a max chord length we use this even if it will not cover all
  -- windows, as they may prefer to focus windows with fewer keys over the ability to focus any
  -- window
  chordLen = if maxUserSelectedLen <= 0 then minCoverLen else min minCoverLen maxUserSelectedLen

-- | A three-state result for handling user-initiated selection cancellation, successful selection,
--   or backspace.
data HandleResult = Exit | Selected Overlay | Backspace

-- | Handle key press events for window selection.
handleKeyboard :: Display -> (Overlay -> X()) -> KeySym -> [Overlay] -> [Overlay] -> X HandleResult
handleKeyboard _ _ _ [] _ = return Exit
handleKeyboard dpy drawFn cancel selected deselected = do
  redraw
  ev <- io $ allocaXEvent $ \e -> do
    maskEvent dpy (keyPressMask .|. keyReleaseMask .|. buttonPressMask) e
    getEvent e
  if | ev_event_type ev == keyPress -> do
         s <- io $ keycodeToKeysym dpy (ev_keycode ev) 0
         if | s == cancel -> return Exit
            | s == xK_BackSpace -> return Backspace
            | isNextOverlayKey s -> handleNextOverlayKey s
            | otherwise -> handleKeyboard dpy drawFn cancel selected deselected
     | ev_event_type ev == buttonPress -> do
         -- See XMonad.Prompt Note [Allow ButtonEvents]
         io $ allowEvents dpy replayPointer currentTime
         handleKeyboard dpy drawFn cancel selected deselected
     | otherwise -> handleKeyboard dpy drawFn cancel selected deselected
 where
  redraw = mapM (mapM_ drawFn) [selected, deselected]
  retryBackspace x =
    case x of
      Backspace -> redraw >> handleKeyboard dpy drawFn cancel selected deselected
      _ -> return x
  isNextOverlayKey keySym = isJust (find ((== Just keySym) . listToMaybe .chord) selected)
  handleNextOverlayKey keySym =
    case fg of
      [x] -> return $ Selected x
      _   -> handleKeyboard dpy drawFn cancel (trim fg) (clear bg) >>= retryBackspace
   where
    (fg, bg) = partition ((== Just keySym) . listToMaybe . chord) selected
    trim = map (\o -> o { chord = tail $ chord o })
    clear = map (\o -> o { chord = [] })
