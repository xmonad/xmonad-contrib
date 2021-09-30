{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.TwoPanePersistent
-- Description :  "XMonad.Layout.TwoPane" with a persistent stack window.
-- Copyright   :  (c) Chayanon Wichitrnithed
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  Chayanon Wichitrnithed <namowi@gatech.edu>
-- Stability   :  unstable
-- Portability :  unportable
--
-- This layout is the same as "XMonad.Layout.TwoPane" except that it keeps track of the slave window
-- that is alongside the master pane. In other words, it prevents the slave pane
-- from changing after the focus goes back to the master pane.

-----------------------------------------------------------------------------


module XMonad.Layout.TwoPanePersistent
  (
    -- * Usage
    -- $usage
  TwoPanePersistent(..)
  ) where

import XMonad.StackSet (focus, up, down, Stack, Stack(..))
import XMonad hiding (focus)

-- $usage
-- Import the module in @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Layout.TwoPanePersistent
--
-- Then add the layout to the @layoutHook@:
--
-- > myLayout = TwoPanePersistent Nothing (3/100) (1/2) ||| Full ||| etc..
-- > main = xmonad def { layoutHook = myLayout }


data TwoPanePersistent a = TwoPanePersistent
  { slaveWin :: Maybe a  -- ^ slave window; if 'Nothing' or not in the current workspace,
                         -- the window below the master will go into the slave pane
  , dFrac :: Rational -- ^ shrink/expand size
  , mFrac :: Rational -- ^ initial master size
  } deriving (Show, Read)


instance (Show a, Eq a) => LayoutClass TwoPanePersistent a where
  doLayout l r s =
    case reverse (up s) of
      -- master is focused
      []         -> return $ focusedMaster l s r

      -- slave is focused
      (master:_) -> return $ focusedSlave l s r master


  pureMessage (TwoPanePersistent w delta split) x =
    case fromMessage x of
      Just Shrink -> Just (TwoPanePersistent w delta (split - delta))
      Just Expand -> Just (TwoPanePersistent w delta (split + delta))
      _ -> Nothing

  description _ = "TwoPanePersistent"


----------------------------------------------------------------------------------------

focusedMaster :: (Eq a) => TwoPanePersistent a -> Stack a -> Rectangle
              -> ( [(a, Rectangle)], Maybe (TwoPanePersistent a) )
focusedMaster (TwoPanePersistent w delta split) s r =
  let (left, right) = splitHorizontallyBy split r in
      case down s of
        -- there exist windows below the master
        (next:_) -> let nextSlave = ( [(focus s, left), (next, right)]
                                    , Just $ TwoPanePersistent (Just next) delta split )
                    in case w of
                      -- if retains state, preserve the layout
                      Just win -> if win `elem` down s && (focus s /= win)
                                  then ( [(focus s, left), (win, right)]
                                       , Just $ TwoPanePersistent w delta split )
                                  else nextSlave
                      -- if no previous state, default to the next slave window
                      Nothing -> nextSlave


        -- the master is the only window
        []       -> ( [(focus s, r)]
                    , Just $ TwoPanePersistent Nothing delta split )



focusedSlave :: TwoPanePersistent a -> Stack a -> Rectangle -> a
             -> ( [(a, Rectangle)], Maybe (TwoPanePersistent a) )
focusedSlave (TwoPanePersistent _ delta split) s r m =
  ( [(m, left), (focus s, right)]
  , Just $ TwoPanePersistent (Just $ focus s) delta split )
  where (left, right) = splitHorizontallyBy split r
