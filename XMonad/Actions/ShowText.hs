{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Actions.ShowText
-- Description :  Display text on the screen.
-- Copyright   :  (c) Mario Pastorelli (2012)
-- License     :  BSD-style (see xmonad/LICENSE)
--
-- Maintainer  :  pastorelli.mario@gmail.com
-- Stability   :  unstable
-- Portability :  unportable
--
-- ShowText displays text for sometime on the screen similar to "XMonad.Util.Dzen"
-- which offers more features (currently)
-----------------------------------------------------------------------------

module XMonad.Actions.ShowText
    ( -- * Usage
      -- $usage
      def
    , handleTimerEvent
    , flashText
    , ShowTextConfig(..)
    ) where

import Data.Map (Map,empty,insert,lookup)
import Prelude hiding (lookup)
import XMonad
import XMonad.Prelude (All, fi, when)
import XMonad.StackSet (current,screen)
import XMonad.Util.Font (Align(AlignCenter)
                       , initXMF
                       , releaseXMF
                       , textExtentsXMF
                       , textWidthXMF)
import XMonad.Util.Timer (startTimer)
import XMonad.Util.XUtils (createNewWindow
                         , deleteWindow
                         , showWindow
                         , paintAndWrite)
import qualified XMonad.Util.ExtensibleState as ES

-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Actions.ShowText
--
-- Then add the event hook handler:
--
-- > xmonad { handleEventHook = myHandleEventHooks <+> handleTimerEvent }
--
-- You can then use flashText in your keybindings:
--
-- > ((modMask, xK_Right), flashText def 1 "->" >> nextWS)
--

-- | ShowText contains the map with timers as keys and created windows as values
newtype ShowText = ShowText (Map Atom Window)
    deriving (Read,Show)

instance ExtensionClass ShowText where
    initialValue = ShowText empty

-- | Utility to modify a ShowText
modShowText :: (Map Atom Window -> Map Atom Window) -> ShowText -> ShowText
modShowText f (ShowText m) = ShowText $ f m

data ShowTextConfig =
    STC { st_font :: String -- ^ Font name
        , st_bg   :: String -- ^ Background color
        , st_fg   :: String -- ^ Foreground color
    }

instance Default ShowTextConfig where
  def =
#ifdef XFT
    STC { st_font = "xft:monospace-20"
#else
    STC { st_font = "-misc-fixed-*-*-*-*-20-*-*-*-*-*-*-*"
#endif
        , st_bg   = "black"
        , st_fg   = "white"
    }

-- | Handles timer events that notify when a window should be removed
handleTimerEvent :: Event -> X All
handleTimerEvent (ClientMessageEvent _ _ _ dis _ mtyp d) = do
    (ShowText m) <- ES.get :: X ShowText
    a <- io $ internAtom dis "XMONAD_TIMER" False
    when (mtyp == a && not (null d))
         (whenJust (lookup (fromIntegral $ head d) m) deleteWindow)
    mempty
handleTimerEvent _ = mempty

-- | Shows a window in the center of the screen with the given text
flashText :: ShowTextConfig
    -> Rational -- ^ number of seconds
    -> String -- ^ text to display
    -> X ()
flashText c i s = do
  f <- initXMF (st_font c)
  d <- asks display
  sc <- gets $ fi . screen . current . windowset
  width <- textWidthXMF d f s
  (as,ds) <- textExtentsXMF f s
  let hight = as + ds
      ht    = displayHeight d sc
      wh    = displayWidth d sc
      y     = (fi ht - hight + 2) `div` 2
      x     = (fi wh - width + 2) `div` 2
  w <- createNewWindow (Rectangle (fi x) (fi y) (fi width) (fi hight))
                      Nothing "" True
  showWindow w
  paintAndWrite w f (fi width) (fi hight) 0 (st_bg c) ""
                (st_fg c) (st_bg c) [AlignCenter] [s]
  releaseXMF f
  io $ sync d False
  t <- startTimer i
  ES.modify $ modShowText (insert (fromIntegral t) w)
