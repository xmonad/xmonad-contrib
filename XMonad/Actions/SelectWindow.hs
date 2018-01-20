-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Actions.SelectWindow
-- Copyright   :  (c) 2017 Tom Hinton <xmonad@larkery.com>
-- License     :  BSD3
--
-- Maintainer  :  Tom Hinton <xmonad@larkery.com>
-- Stability   :  unstable
-- Portability :  unportable
--
-- Select windows using the keyboard quickly.
-----------------------------------------------------------------------------

module XMonad.Actions.SelectWindow (selectWindow,
                                     selectWindowColors,
                                     selectWindowKeys) where

import Graphics.X11.Xlib.Extras (getWindowAttributes,
                                 WindowAttributes (..))

import XMonad.Util.Font
import XMonad.Util.XUtils
import Data.List (sortOn)
import qualified Debug.Trace as D

import XMonad
import qualified XMonad.StackSet as W
import Control.Arrow ((&&&))

-- $usage
-- You can use this module to focus, move, delete etc. windows using hints.
-- It's a bit like ace-window in emacs, if you have used that. For example
-- > import XMonad.Actions.SelectWindow
-- in your keybindings
-- > ("M-<Space>", selectWindow >>= (flip whenJust (windows . W.focusWindow)))
-- to select a window and focus it using a keypress.
-- Equally you could say
-- > selectWindow >>= (flip whenJust killWindow)
-- To delete a window with a keypress.

-- | Select a window by pressing a key displayed on a hint above the window
-- and return it, or Nothing if you press another key.
-- See "selectWindowKeys" for all the options.
selectWindow :: X (Maybe Window)
selectWindow = selectWindowColors "white" "black"

-- | Select a window using you keyboard, giving colours for the hints.
-- See "selectWindowKeys" for more details.
-- The two strings are foreground and background color for the hint overlays.
--
-- Returns "Just" the selected window or Nothing if you press Escape
-- or an unused key.
selectWindowColors :: String -> String -> X (Maybe Window)
selectWindowColors = selectWindowKeys ["asdfgzxcv", "hjkl;nm,.", "qwertyuiop" ] "xft:Sans-24"

-- | Select a window from on the screen using the keyboard.
-- The first argument should contain a string for each physical monitor you want to be able to use.
-- For example, if it is
-- > ["abc", "def", "ghi"]
-- The first three visible windows on the leftmost screen will be selectable with keys a,b,c
-- The first three visible windows on the next leftmost screen with d,e,f, and so on.
-- The next three strings are the font, foreground & background colours for the hint windows
-- Return "Just" the selected window, or Nothing if you press a key not in the list.
selectWindowKeys :: [String] -> String -> String -> String -> X (Maybe Window)
selectWindowKeys keyss fontName fg bg = withDisplay $ \dpy -> do
  allWindows <- gets (map (W.integrate' . W.stack . W.workspace) . (sortOn ((rect_x &&& rect_y) . screenRect . W.screenDetail)) . (uncurry (:)) . (W.current &&& W.visible) . windowset)
  atts <- io $ mapM (mapM (getWindowAttributes dpy)) allWindows

  let windowCenter (win, (WindowAttributes {wa_x = x, wa_y = y, wa_height = h, wa_width = w})) =
        (win, (x + (w`div`2), y + (h`div`2)))

      windowVisible (_, (WindowAttributes {wa_map_state = ms})) = ms == waIsViewable

      wins = map (map windowCenter) $ map (filter windowVisible) $ map (uncurry zip) $ zip allWindows atts

  -- create a lot of little windows to select with

  font <- initXMF fontName
  (extent1, extent2) <- textExtentsXMF font (concat keyss)

  let winPos = map (sortOn snd) wins -- these are sorted within ws but not without!
      winHeight = fromIntegral $ extent1 + extent2 + 4
      winHalfHeight = fromIntegral $ ceiling $ (fromIntegral winHeight) / 2
      winKeys = concatMap (uncurry zip) $ zip keyss winPos
      pop (k, (w, (x, y))) = do
        win <- createNewWindow (Rectangle (fromIntegral x-winHalfHeight) (fromIntegral y-winHalfHeight) winHeight winHeight) Nothing bg False
        showWindow win
        paintAndWrite win font winHeight winHeight 2 bg fg fg bg [AlignCenter] [[k]]
        return $ (win, k)

      readKey = do
        (e, _) <- nextKeyEvent dpy
        case e of
          (Press mask sym (k:_)) -> return $ fst <$> lookup k winKeys
          Expose -> readKey
          _ -> readKey

  XConf { theRoot = root } <- ask
  status <- io $ grabKeyboard dpy root True grabModeAsync grabModeAsync currentTime
  selection <- if (status == grabSuccess)
    then do displays <- mapM pop winKeys
            selection <- readKey
            mapM_ (\(w, _) -> deleteWindow w) displays
            io $ ungrabKeyboard dpy currentTime
            return selection
    else return Nothing
  releaseXMF font
  return selection

-- | Represents a key event or an expose event, or some other thing, for use in the read loop
data KEvent = Expose | Press KeyMask KeySym String | Release KeySym | Skip deriving (Show)

-- | Get the next KEvent and the Event it came from from the display.
nextKeyEvent :: Display -> X (KEvent, Event)
nextKeyEvent d = do
  io $ do allocaXEvent $ \e -> do
            maskEvent d (exposureMask .|. keyPressMask .|. keyReleaseMask) e
            ev <- getEvent e
            fmap (flip (,) ev) $
              if ev_event_type ev == keyPress then
                do x <- lookupString $ asKeyEvent e
                   return $ case x of (Just ks, str) -> Press (ev_state ev) ks str
                                      _ -> Skip
              else if ev_event_type ev == keyRelease then
                     do s <- keycodeToKeysym d (ev_keycode ev) 0
                        return $ Release s
              else if ev_event_type ev == expose && ev_count ev == 0 then
                     return $ Expose
              else return Skip
