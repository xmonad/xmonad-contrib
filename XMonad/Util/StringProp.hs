-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Util.StringProp
-- Description :  Internal utility functions for storing Strings with the root window.
-- Copyright   :  (c) Nicolas Pouillard 2009
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Nicolas Pouillard <nicolas.pouillard@gmail.com>
-- Stability   :  unstable
-- Portability :  unportable
--
-- Internal utility functions for storing Strings with the root window.
--
-- Used for global state like IORefs with string keys, but more latency,
-- persistent between xmonad restarts.

module XMonad.Util.StringProp (
    StringProp,
    getStringProp, setStringProp,
    getStringListProp, setStringListProp,
    ) where

import XMonad
import Foreign.C.String (castCCharToChar,castCharToCChar)

type StringProp = String

withStringProp :: (MonadIO m) => StringProp -> Display -> (Window -> Atom -> m b) -> m b
withStringProp prop dpy f = do
    rootw <- io $ rootWindow dpy $ defaultScreen dpy
    a     <- io $ internAtom dpy prop False
    f rootw a

-- | Set the value of a string property.
setStringProp :: (MonadIO m) => Display -> StringProp -> [Char] -> m ()
setStringProp dpy prop string =
  withStringProp prop dpy $ \rootw a ->
    io $ changeProperty8 dpy rootw a a propModeReplace $ map castCharToCChar string

-- | Get the name of a string property and returns it as a 'Maybe'.
getStringProp :: (MonadIO m) => Display -> StringProp -> m (Maybe [Char])
getStringProp dpy prop =
  withStringProp prop dpy $ \rootw a -> do
    p <- io $ getWindowProperty8 dpy a rootw
    return $ map castCCharToChar <$> p

-- | Given a property name, returns its contents as a list. It uses the empty
-- list as default value.
getStringListProp :: (MonadIO m) => Display -> StringProp -> m [String]
getStringListProp dpy prop = maybe [] words <$> getStringProp dpy prop

-- | Given a property name and a list, sets the value of this property with
-- the list given as argument.
setStringListProp :: (MonadIO m) => Display -> StringProp -> [String] -> m ()
setStringListProp dpy prop str = setStringProp dpy prop (unwords str)
