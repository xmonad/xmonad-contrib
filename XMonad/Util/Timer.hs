-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Util.Timer
-- Copyright   :  (c) Andrea Rossato and David Roundy 2007
-- License     :  BSD-style (see xmonad/LICENSE)
--
-- Maintainer  :  andrea.rossato@unibz.it
-- Stability   :  unstable
-- Portability :  unportable
--
-- A module for setting up timers
-----------------------------------------------------------------------------

module XMonad.Util.Timer
    ( -- * Usage
      -- $usage
      startTimer
    , handleTimer
    , TimerId
    ) where

import XMonad
import Control.Concurrent
import Data.Unique

-- $usage
-- This module can be used to setup a timer to handle deferred events.
-- See 'XMonad.Layout.ShowWName' for an usage example.

type TimerId = Int

-- | Start a timer, which will send a ClientMessageEvent after some
-- time (in seconds).
startTimer :: Rational -> X TimerId
startTimer s = io $ do
  u   <- hashUnique <$> newUnique
  xfork $ do
    d   <- openDisplay ""
    rw  <- rootWindow d $ defaultScreen d
    threadDelay (fromEnum $ s * 1000000)
    a <- internAtom d "XMONAD_TIMER" False
    allocaXEvent $ \e -> do
         setEventType e clientMessage
         setClientMessageEvent e rw a 32 (fromIntegral u) 0
         sendEvent d rw False structureNotifyMask e
    sync d False
  return u

-- | Given a 'TimerId' and an 'Event', run an action when the 'Event'
-- has been sent by the timer specified by the 'TimerId'
handleTimer :: TimerId -> Event -> X (Maybe a) -> X (Maybe a)
handleTimer ti (ClientMessageEvent {ev_message_type = mt, ev_data = dt}) action = do
  d <- asks display
  a <- io $ internAtom d "XMONAD_TIMER" False
  if mt == a && dt /= [] && fromIntegral (head dt) == ti
     then action
     else return Nothing
handleTimer _ _ _ = return Nothing
