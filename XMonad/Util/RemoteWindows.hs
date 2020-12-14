-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Util.RemoteWindows
-- Copyright   :  (c) Anton Vorontsov <anton@enomsg.org> 2014
-- License     :  BSD-style (as xmonad)
--
-- Maintainer  :  Anton Vorontsov <anton@enomsg.org>
-- Stability   :  unstable
-- Portability :  unportable
--
-- This module implements a proper way of finding out whether the window
-- is remote or local.
--
-- Just checking for a hostname and WM_CLIENT_MACHINE being equal is often
-- not enough because the hostname is a changing subject (without any
-- established notification mechanisms), and thus WM_CLIENT_MACHINE and
-- the hostname can diverge even for a local window.
--
-- This module solves the problem. As soon as there is a new window
-- created, we check the hostname and WM_CLIENT_MACHINE, and then we cache
-- the result into the XMONAD_REMOTE property.
--
-- Notice that XMonad itself does not know anything about hostnames, nor
-- does it have any dependency on Network.* modules. For this module it is
-- not a problem: you can provide a mean to get the hostname through your
-- config file (see usage). Or, if you don't like the hassle of handling
-- dynamic hostnames (suppose your hostname never changes), it is also
-- fine: this module will fallback to using environment variables.
--
-----------------------------------------------------------------------------

module XMonad.Util.RemoteWindows
    ( -- $usage
      isLocalWindow
    , manageRemote
    , manageRemoteG
    ) where

import XMonad
import XMonad.Util.WindowProperties
import Data.Maybe
import Control.Monad
import System.Posix.Env

-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad
-- > import XMonad.Util.RemoteWindows
-- > import Network.BSD
-- >
-- > main = xmonad def
-- >    { manageHook = manageRemote =<< io getHostName }

guessHostName :: IO String
guessHostName = pickOneMaybe <$> (getEnv `mapM` vars)
  where
    pickOneMaybe = last . (mzero:) . take 1 . catMaybes
    vars = ["XAUTHLOCALHOSTNAME","HOST","HOSTNAME"]

setRemoteProp :: Window -> String -> X ()
setRemoteProp w host = do
    d <- asks display
    p <- getAtom "XMONAD_REMOTE"
    v <- hasProperty (Machine host) w
    io $ changeProperty32 d w p cARDINAL propModeReplace
                          [fromIntegral . fromEnum $ not v]

-- | Given a window, tell if it is a local or a remote process. Normally,
-- it checks XMONAD_REMOTE property. If it does not exist (i.e. the
-- manageRemote hook was not deployed in user's config), it falls back to
-- checking environment variables and assuming that hostname never
-- changes.
isLocalWindow :: Window -> X Bool
isLocalWindow w = getProp32s "XMONAD_REMOTE" w >>= \p -> case p of
    Just [y] -> return $ y == 0
    _ -> io guessHostName >>= \host -> hasProperty (Machine host) w

-- | Use this hook to let XMonad properly track remote/local windows. For
-- example, @manageHook = manageRemote =<< io getHostName@.
manageRemote :: String -> ManageHook
manageRemote host = ask >>= \w -> liftX (setRemoteProp w host) >> return mempty

-- | Use this hook if you want to manage XMONAD_REMOTE properties, but
-- don't want to use an external getHostName in your config. That way you
-- are retreating to environment variables.
manageRemoteG :: ManageHook
manageRemoteG = manageRemote =<< io guessHostName
