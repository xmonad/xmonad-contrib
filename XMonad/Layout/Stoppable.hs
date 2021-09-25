{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances #-}
{-# LANGUAGE PatternGuards #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.Stoppable
-- Description :  A layout modifier to stop all non-visible processes.
-- Copyright   :  (c) Anton Vorontsov <anton@enomsg.org> 2014
-- License     :  BSD-style (as xmonad)
--
-- Maintainer  :  Anton Vorontsov <anton@enomsg.org>
-- Stability   :  unstable
-- Portability :  unportable
--
-- This module implements a special kind of layout modifier, which when
-- applied to a layout, causes xmonad to stop all non-visible processes.
-- In a way, this is a sledge-hammer for applications that drain power.
-- For example, given a web browser on a stoppable workspace, once the
-- workspace is hidden the web browser will be stopped.
--
-- Note that the stopped application won't be able to communicate with X11
-- clipboard. For this, the module actually stops applications after a
-- certain delay, giving a chance for a user to complete copy-paste
-- sequence. By default, the delay equals to 15 seconds, it is
-- configurable via 'Stoppable' constructor.
--
-- The stoppable modifier prepends a mark (by default equals to
-- \"Stoppable\") to the layout description (alternatively, you can choose
-- your own mark and use it with 'Stoppable' constructor). The stoppable
-- layout (identified by a mark) spans to multiple workspaces, letting you
-- to create groups of stoppable workspaces that only stop processes when
-- none of the workspaces are visible, and conversely, unfreezing all
-- processes even if one of the stoppable workspaces are visible.
--
-- To stop the process we use signals, which works for most cases. For
-- processes that tinker with signal handling (debuggers), another
-- (Linux-centric) approach may be used. See
-- <https://www.kernel.org/doc/Documentation/cgroups/freezer-subsystem.txt>
--
-- * Note
-- This module doesn't work on programs that do fancy things with processes
-- (such as Chromium) and programs that do not set _NET_WM_PID.
-----------------------------------------------------------------------------

module XMonad.Layout.Stoppable
    ( -- $usage
      Stoppable(..)
    , stoppable
    ) where

import XMonad
import XMonad.Prelude
import XMonad.Actions.WithAll
import XMonad.Util.WindowProperties
import XMonad.Util.RemoteWindows
import XMonad.Util.Timer
import XMonad.StackSet hiding (filter)
import XMonad.Layout.LayoutModifier
import System.Posix.Signals

-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad
-- > import XMonad.Layout.Stoppable
-- >
-- > main = xmonad def
-- >    { layoutHook = layoutHook def ||| stoppable (layoutHook def) }
--
-- Note that the module has to distinguish between local and remote
-- proccesses, which means that it needs to know the hostname, so it looks
-- for environment variables (e.g. HOST).
--
-- Environment variables will work for most cases, but won't work if the
-- hostname changes. To cover dynamic hostnames case, in addition to
-- layoutHook you have to provide manageHook from
-- "XMonad.Util.RemoteWindows" module.
--
-- For more detailed instructions on editing the layoutHook see:
--
-- "XMonad.Doc.Extending#Editing_the_layout_hook"

signalWindow :: Signal -> Window -> X ()
signalWindow s w = do
    pid <- getProp32s "_NET_WM_PID" w
    io $ (signalProcess s . fromIntegral) `mapM_` fromMaybe [] pid

signalLocalWindow :: Signal -> Window -> X ()
signalLocalWindow s w  = isLocalWindow w >>= flip when (signalWindow s w)

withAllOn :: (a -> X ()) -> Workspace i l a -> X ()
withAllOn f wspc = f `mapM_` integrate' (stack wspc)

withAllFiltered :: (Workspace i l a -> Bool)
                -> [Workspace i l a]
                -> (a -> X ()) -> X ()
withAllFiltered p wspcs f = withAllOn f `mapM_` filter p wspcs

sigStoppableWorkspacesHook :: String -> X ()
sigStoppableWorkspacesHook k = do
    ws <- gets windowset
    withAllFiltered isStoppable (hidden ws) (signalLocalWindow sigSTOP)
  where
    isStoppable ws = k `elem` words (description $ layout ws)

-- | Data type for ModifiedLayout. The constructor lets you to specify a
-- custom mark/description modifier and a delay. You can also use
-- 'stoppable' helper function.
data Stoppable a = Stoppable
    { mark :: String
    , delay :: Rational
    , timer :: Maybe TimerId
    } deriving (Show,Read)

instance LayoutModifier Stoppable Window where
    modifierDescription = mark

    hook _   = withAll $ signalLocalWindow sigCONT

    handleMess (Stoppable m _ (Just tid)) msg
        | Just ev <- fromMessage msg = handleTimer tid ev run
          where run = sigStoppableWorkspacesHook m >> return Nothing
    handleMess (Stoppable m d _) msg
        | Just Hide <- fromMessage msg =
            Just . Stoppable m d . Just <$> startTimer d
        | otherwise = return Nothing

-- | Convert a layout to a stoppable layout using the default mark
-- (\"Stoppable\") and a delay of 15 seconds.
stoppable :: l a -> ModifiedLayout Stoppable l a
stoppable = ModifiedLayout (Stoppable "Stoppable" 15 Nothing)
