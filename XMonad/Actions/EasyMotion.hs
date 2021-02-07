{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Actions.EasyMotion
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
-- Inspired by https://github.com/easymotion/vim-easymotion.
-- Thanks to Tom Hinton (https://github.com/larkery) for some feature inspiration and window
-- sorting code.
--
-----------------------------------------------------------------------------

module XMonad.Actions.EasyMotion (
                                   -- * Usage
                                   -- $usage
                                   selectWindow
                                 , def
                                 , EasyMotionConfig(..)
                                 , fullSize
                                 , fixedSize
                                 , textSize
                                 , bar
                                 ) where

import           XMonad
import           XMonad.StackSet          as W
import           XMonad.Util.Font         (releaseXMF, initXMF, Align(AlignCenter), XMonadFont(..), textExtentsXMF)
import           XMonad.Util.XUtils       (fi, createNewWindow, paintAndWrite, deleteWindow, showWindow)
import           Control.Monad            (replicateM)
import           Control.Arrow            ((&&&))
import           Data.Maybe               (isJust)
import           Data.Set                 (fromList, toList)
import           Graphics.X11.Xlib.Extras (getWindowAttributes, getEvent)
import qualified Data.List as L           (filter, foldl', partition, find, nub)
import           Data.List                (sortOn)

-- $usage
--
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- >    import XMonad.Actions.EasyMotion (selectWindow)
--
-- Then add a keybinding and an action to the selectWindow function. In this case M-f to focus:
--
-- >    , ((modm, xK_f), (selectWindow def) >>= (flip whenJust (windows . W.focusWindow)))
--
-- Similarly, to kill a window with M-f:
--
-- >    , ((modm, xK_f), (selectWindow def) >>= (flip whenJust killWindow))
--
-- Default chord keys are s,d,f,j,k,l. To customise these and display options assign
-- different values to def:
--
-- >    import XMonad.Actions.EasyMotion (selectWindow, EasyMotionConfig(..))
-- >    , ((modm, xK_f), (selectWindow def {sKeys = [[xK_f, xK_d]]}) >>= (flip whenJust (windows . W.focusWindow)))
--
-- You must supply at least two different keys in the sKeys list.
--
-- To map different sets of keys to different screens:
--
-- >    import XMonad.Actions.EasyMotion (selectWindow, EasyMotionConfig(..))
-- >    , ((modm, xK_f), (selectWindow def {sKeys = [[xK_f, xK_d], [xK_j, xK_k]}) >>= (flip whenJust (windows . W.focusWindow)))
--
-- To customise font:
--
-- >    import XMonad.Actions.EasyMotion (selectWindow, EasyMotionConfig(..))
-- >    , ((modm, xK_f), (selectWindow def {font = "xft: Sans-40"}) >>= (flip whenJust (windows . W.focusWindow)))
--
-- The font field provided is supplied directly to the initXMF function. The default is
-- "xft:Sans-100". Some example options:
--
-- >    "xft: Sans-40"
-- >    "xft: Arial-100"
-- >    "xft: Cambria-80"
--
-- Customise the overlay by supplying a function to do so. The signature is @'Position' ->
-- 'Rectangle' -> 'X' 'Rectangle'@. The parameters are the height in pixels of the selection chord
-- and the rectangle of the window to be overlaid. Some are provided:
--
-- >    import XMonad.Actions.EasyMotion (selectWindow, EasyMotionConfig(..), proportional, bar, fullSize)
-- >    , ((modm, xK_f), (selectWindow def { overlayF = proportional 0.3 }) >>= (flip whenJust (windows . W.focusWindow)))
-- >    , ((modm, xK_f), (selectWindow def { overlayF = bar 0.5 }) >>= (flip whenJust (windows . W.focusWindow)))
-- >    , ((modm, xK_f), (selectWindow def { overlayF = fullSize }) >>= (flip whenJust (windows . W.focusWindow)))
-- >    , ((modm, xK_f), (selectWindow def { overlayF = fixedSize 300 350 }) >>= (flip whenJust (windows . W.focusWindow)))

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

-- | Associates a user window, an overlay window created by this module, a rectangle circumscribing
--   these windows, and the chord that will be used to select them
data Overlay =
  Overlay { win     :: Window           -- ^ The window managed by xmonad
          , attrs   :: WindowAttributes -- ^ Window attributes for 'win
          , overlay :: Window           -- ^ Our window used to display the overlay
          , rect    :: Rectangle        -- ^ The rectangle of 'overlay
          , chord   :: [KeySym]         -- ^ The chord we'll display in the overlay
          }

-- | Configuration options for EasyMotion. sKeys can come in two forms, [[all keys here]] or [[keys
--   for screen 1 here],[keys for screen 2 here],...]. In the first form, all keys will be used for
--   windows on every screen. In the second form, keys will map to screens based on screen
--   position. If the number of windows for which chords are required exceeds maxChordLen, chords
--   will simply not be generated for these windows. Thus single-key selection may be preferred
--   over the ability to select any window. @cancelKey@, @xK_BackSpace@ and any duplicates will be
--   removed from @sKeys@ if included.
data EasyMotionConfig =
  EMConf { txtCol      :: String                             -- ^ Color of the text displayed
         , bgCol       :: String                             -- ^ Color of the window overlaid
         , overlayF    :: Position -> Rectangle -> Rectangle -- ^ Function to generate overlay rectangle
         , borderCol   :: String                             -- ^ Color of the overlay window borders
         , sKeys       :: [[KeySym]]                         -- ^ Keys to use for window selection
         , cancelKey   :: KeySym                             -- ^ Key to use to cancel selection
         , font        :: String                             -- ^ Font for selection characters (passed to initXMF)
         , borderPx    :: Int                                -- ^ Width of border in pixels
         , maxChordLen :: Int                                -- ^ Maximum chord length. Use 0 for no maximum.
         }

instance Default EasyMotionConfig where
  def =
    EMConf { txtCol      = "#ffffff"
           , bgCol       = "#000000"
           , overlayF    = proportional (0.3::Double)
           , borderCol   = "#ffffff"
           , sKeys       = [[xK_s, xK_d, xK_f, xK_j, xK_k, xK_l]]
           , cancelKey   = xK_q
           , borderPx    = 1
           , maxChordLen = 0
#ifdef XFT
           , font        = "xft:Sans-100"
#else
           , font        = "-misc-fixed-*-*-*-*-200-*-*-*-*-*-*-*"
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
                         -- clamp f in [0,1] as other values will appear to lock up xmonad
                         -- as the overlay will be displayed off-screen
  where f' = min 0.0 $ max f 1.0

-- | Handles overlay display and window selection. Called after config has been sanitised.
handleSelectWindow :: EasyMotionConfig -> X (Maybe Window)
handleSelectWindow EMConf { sKeys = [] } = return Nothing
handleSelectWindow c = do
  f <- initXMF $ font c
  th <- textExtentsXMF f (concatMap keysymToString (concat $ sKeys c)) >>= \(asc, dsc) -> return $ asc + dsc + 2
  XConf { theRoot = rw, display = dpy } <- ask
  XState { mapped = mappedWins, windowset = ws } <- get
  let currentW = W.stack . W.workspace . W.current $ ws
      -- Bucket windows by screen, unless the user has provided only a single list of keys
      winBuckets :: X [[Overlay]]
      winBuckets = sequence $ fmap (sequence . fmap buildOverlay) $
        case sKeys c of
          [_] -> [toList mappedWins]
          _ -> map (L.filter (`elem` (toList mappedWins)) . W.integrate' . W.stack . W.workspace) sortedScreens
            where
              sortedScreens = sortOn ((rect_x &&& rect_y) . screenRect . W.screenDetail) (W.current ws : W.visible ws)
      -- Zip window buckets with selection keys, then sort them within screens
      sortedOverlays :: X [[Overlay]]
      sortedOverlays = fmap (zipWith (appendChords (maxChordLen c)) (sKeys c) . fmap (sortOn ((wa_x &&& wa_y) . attrs))) winBuckets
      displayF = displayOverlay f (bgCol c) (borderCol c) (txtCol c) (borderPx c)
      buildOverlay :: Window -> X Overlay
      buildOverlay w = do
        wAttrs <- io $ getWindowAttributes dpy w
        let r = overlayF c th $ makeRect wAttrs
        o <- createNewWindow r Nothing "" True
        return Overlay { rect=r, overlay=o, win=w, attrs=wAttrs, chord=[] }
  overlays <- fmap concat sortedOverlays
  status <- io $ grabKeyboard dpy rw True grabModeAsync grabModeAsync currentTime
  if (status == grabSuccess)
    then do
      resultWin <- handleKeyboard dpy displayF (cancelKey c) overlays []
      io $ ungrabKeyboard dpy currentTime
      mapM_ (deleteWindow . overlay) overlays
      io $ sync dpy False
      releaseXMF f
      case resultWin of
        Selected o -> return . Just $ win o
        _ -> whenJust currentW (windows . W.focusWindow . W.focus) >> return Nothing -- return focus correctly
    else releaseXMF f >> return Nothing

-- | Display overlay windows and chords for window selection
selectWindow :: EasyMotionConfig -> X (Maybe Window)
selectWindow conf =
  handleSelectWindow conf { sKeys = map sanitiseKeys (sKeys conf) }
    where
      -- make sure the key lists don't contain: 'cancelKey, backspace, or duplicates
      sanitiseKeys = toList . fromList . L.nub . L.filter (`notElem` [cancelKey conf, xK_BackSpace])

-- | Take a list of overlays lacking chords, return a list of overlays with key chords
appendChords :: Int -> [KeySym] -> [Overlay] -> [Overlay]
appendChords _ [] _ = []
appendChords maxLen ks overlays =
  zipWith (\o c -> o { chord=c }) overlays chords
    where
      chords = replicateM chordLen ks
      tempLen = -((-(length overlays)) `div` length ks)
      chordLen = if maxLen <= 0 then tempLen else min tempLen maxLen

-- | Get a key event, translate it to an event type and keysym
event :: Display -> IO (EventType, KeySym)
event d = allocaXEvent $ \e -> do
  maskEvent d (keyPressMask .|. keyReleaseMask) e
  KeyEvent {ev_event_type = t, ev_keycode = c} <- getEvent e
  s <- keycodeToKeysym d c 0
  return (t, s)

-- | A three-state result for handling user-initiated selection cancellation, successful selection,
--   or backspace.
data HandleResult = Exit | Selected Overlay | Backspace
-- | Handle key press events for window selection.
handleKeyboard :: Display -> (Overlay -> X()) -> KeySym -> [Overlay] -> [Overlay] -> X (HandleResult)
handleKeyboard _ _ _ [] _ = return Exit
handleKeyboard dpy drawFn cancel fgOverlays bgOverlays = do
  let redraw = mapM (mapM_ drawFn) [fgOverlays, bgOverlays]
  let retryBackspace x =
        case x of
          Backspace -> redraw >> handleKeyboard dpy drawFn cancel fgOverlays bgOverlays
          _ -> return x
  redraw
  (t, s) <- io $ event dpy
  case () of
    () | t == keyPress && s == cancel -> return Exit
    () | t == keyPress && s == xK_BackSpace -> return Backspace
    () | t == keyPress && isJust (L.find ((== s) . head .chord) fgOverlays) ->
      case fg of
        [x] -> return $ Selected x
        _   -> handleKeyboard dpy drawFn cancel (trim fg) (clear bg) >>= retryBackspace
      where
        (fg, bg) = L.partition ((== s) . head . chord) fgOverlays
        trim = map (\o -> o { chord = tail $ chord o })
        clear = map (\o -> o { chord = [] })
    () -> handleKeyboard dpy drawFn cancel fgOverlays bgOverlays

-- | Create a rectangle from window attributes
makeRect :: WindowAttributes -> Rectangle
makeRect wa = Rectangle (fi (wa_x wa)) (fi (wa_y wa)) (fi (wa_width wa)) (fi (wa_height wa))

-- | Display an overlay with the provided formatting
displayOverlay :: XMonadFont -> String -> String -> String -> Int -> Overlay -> X ()
displayOverlay f bgC brC textC brW Overlay { overlay = w, rect = r, chord = c } = do
  showWindow w
  paintAndWrite w f (fi (rect_width r)) (fi (rect_height r)) (fi brW) bgC brC textC bgC [AlignCenter] [L.foldl' (++) "" $ map keysymToString c]
