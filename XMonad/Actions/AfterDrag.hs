-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Actions.AfterDrag
-- Description :  Allows you to add actions dependent on the current mouse drag.
-- Copyright   :  (c) 2014 Anders Engstrom <ankaan@gmail.com>
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  Anders Engstrom <ankaan@gmail.com>
-- Stability   :  unstable
-- Portability :  unportable
--
-- Perform an action after the current mouse drag is completed.
-----------------------------------------------------------------------------

module XMonad.Actions.AfterDrag (
                -- * Usage
                -- $usage
                afterDrag,
                ifClick,
                ifClick') where

import XMonad

import Data.Time (NominalDiffTime, diffUTCTime, getCurrentTime)

-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- >    import XMonad.Actions.AfterDrag
--
-- Then add appropriate mouse bindings, for example:
--
-- >        , ((modm, button3), (\w -> focus w >> mouseResizeWindow w >> ifClick (windows $ W.float w $ W.RationalRect 0 0 1 1)))
--
-- This will allow you to resize windows as usual, but if you instead of
-- draging click the mouse button the window will be automatically resized to
-- fill the whole screen.
--
-- For detailed instructions on editing your mouse bindings, see
-- "XMonad.Doc.Extending#Editing_mouse_bindings".
--
-- More practical examples are available in "XMonad.Actions.FloatSnap".

-- | Schedule a task to take place after the current dragging is completed.
afterDrag
    :: X () -- ^ The task to schedule.
    -> X ()
afterDrag task = do drag <- gets dragging
                    case drag of
                        Nothing -> return () -- Not dragging
                        Just (motion, cleanup) -> modify $ \s -> s { dragging = Just(motion, cleanup >> task) }

-- | Take an action if the current dragging can be considered a click,
--   supposing the drag just started before this function is called.
--   A drag is considered a click if it is completed within 300 ms.
ifClick
  :: X ()   -- ^ The action to take if the dragging turned out to be a click.
  -> X ()
ifClick action = ifClick' 300 action (return ())

-- | Take an action if the current dragging is completed within a certain time (in milliseconds.)
ifClick'
  :: Int    -- ^ Maximum time of dragging for it to be considered a click (in milliseconds.)
  -> X ()   -- ^ The action to take if the dragging turned out to be a click.
  -> X ()   -- ^ The action to take if the dragging turned out to not be a click.
  -> X ()
ifClick' ms click drag = do
  start <- io getCurrentTime
  afterDrag $ do
    stop <- io getCurrentTime
    if diffUTCTime stop start <= (fromIntegral ms / 10^(3 :: Integer) :: NominalDiffTime)
      then click
      else drag
