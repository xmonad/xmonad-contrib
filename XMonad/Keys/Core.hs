{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-|

Module      : XMonad.Keys.Core
Description :
Copyright   : (C) David Janssen, 2016
License     : BSD3
Maintainer  : David Janssen <janssen.dhj@gmail.com>
Stability   : unstable
Portability : unportable

This module currently contains the rudimentary core functionality for
XMonad.Keys. Plans are to expand the utility of this module to ease working with
KeyEvents, (KeyMask, KeySym) style data, and KeyMaps. Currently, this module is
just the result of refactoring EZConfig and the Menu functionality in
XMonad.Actions.Menu.

Future refactoring opportunities:
- X.A.SubMap
- plenty others

-}

module XMonad.Keys.Core where

import           Control.Arrow ((&&&))
import           Data.Bits     ((.|.))
import qualified Data.Map      as M
import           Data.Maybe    (fromMaybe)
import           XMonad

type KeyMap a   = M.Map KeyStroke a

data KeyStroke = KeyStroke { mask :: KeyMask
                           , sym  :: KeySym } deriving (Show, Eq, Ord)

newtype KB a    = KB (X a)
  deriving (Functor, Applicative, Monoid, Default, Monad, MonadIO,
            MonadState XState, MonadReader XConf, Typeable)


-- | Try grabbing the keyboard and running an X action in the context of the
--   grabbed keyboard. If the keyboard grab fails, return Nothing, otherwise
--   return the result of running the X () action. Note, this makes it
--   impossible to distinguish between keyboard-grab failure and (userCode x)
--   returning Nothing.
withKB :: KB a -> X (Maybe a)
withKB (KB x) = do
  XConf { display = dpy, theRoot = win } <- ask
  status <- io $ grabKeyboard dpy win False grabModeAsync grabModeAsync currentTime
  if status == grabSuccess then
    do out <- userCode x
       io $ ungrabKeyboard dpy currentTime
       return out
    else
       return Nothing

-- I'd like to refactor the next two functions to avoid code duplication, but
-- you have to perform the lookupString while you still have access to the
-- XEventPtr, requiring a slightly different approach. This is future work.

-- | Wait for and return the next KeyEvent
waitEvent :: KB Event
waitEvent = KB $ do
  withDisplay $ \dpy -> io $
    allocaXEvent $ \e -> do
      maskEvent dpy (keyPressMask .|. keyReleaseMask) e
      getEvent e

-- | waitKey returns the next KeyStroke that occurs
waitStroke :: KB KeyStroke
waitStroke = KB $ do
  (sym, e) <- withDisplay $ \dpy -> io $
    allocaXEvent $ \e -> do
      maskEvent dpy keyPressMask e
      ev <- getEvent e
      (ks, _) <- lookupString $ asKeyEvent e
      return (fromMaybe xK_VoidSymbol ks, ev)
  cleaned <- cleanMask . ev_state $ e
  return $ KeyStroke cleaned sym

-- Utility functions for working with KeyStroke data
toKeyStroke :: (KeyMask, KeySym) -> KeyStroke
toKeyStroke = uncurry KeyStroke

fromKeyStroke :: KeyStroke -> (KeyMask, KeySym)
fromKeyStroke = mask &&& sym


-- The following code deals with Mappings from KeyStrokes to anything else, and
-- provides functionality to 'run' such maps by waiting for keyboard input.

-- | getItem looks up the next keystroke in the KeyMap
getItem :: KeyMap a -> KB (Maybe a)
getItem m = waitStroke >>= \k -> return $ M.lookup k m

-- | waitItem waits until a matched keystroke is found and looks that up
waitItem :: KeyMap a -> KB a
waitItem m = do
  x <- getItem m
  case x of
    Just a  -> return a
    Nothing -> waitItem m
