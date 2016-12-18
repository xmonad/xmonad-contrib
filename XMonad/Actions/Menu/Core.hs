{-|

Module      : XMonad.Actions.Menu.Core
Description : The core functionality that runs menus.
Copyright   : (C) David Janssen, 2016
License     : BSD3
Maintainer  : David Janssen <janssen.dhj@gmail.com>
Stability   : unstable
Portability : unportable

This module contains the code that is responsible for listening to the keyboard
and running mappings between key-strokes and actions.

-}

module XMonad.Actions.Menu.Core

  (
    Item (..)
  , Menu (..)
  , Renderer
  , runMenu
  )


where

import           Control.Applicative ((<$>))
import           Control.Arrow       ((&&&))
import           Control.Monad       (void)
import qualified Data.Map            as M
import           XMonad
import           XMonad.Keys.Core    (KeyStroke, waitItem, withKB)

type Renderer a = Menu a -> X (X ())


data Item a = Item { keyStroke :: KeyStroke
                   , descr     :: String
                   , action    :: X ()
                   , tags      :: a }

data Menu a = Menu { renderF :: Renderer a
                   , items   :: [Item a] }


-- | runMenu waits for a recognized KeyStroke, then runs the X ().
runMenu :: Menu a -> X ()
runMenu m = do
  closer <- renderF m m
  let actions = M.fromList . map (keyStroke &&& action) . items $ m
  act    <- withKB . waitItem $ actions
  _      <- userCode closer
  whenJust act (void <$> userCode)
  dpy    <- asks display
  io $ sync dpy False
  return ()

-- | waitKey returns the next KeyStroke that occurs
waitKey :: X KeyStroke
waitKey = do
  XConf { display=dpy } <- ask
  (sym, e) <- io $
    -- X-stuff to grab the next (Modifiers, KeyPress) from the system
    allocaXEvent $ \e -> do
        maskEvent dpy keyPressMask e
        ev <- getEvent e
        (ks, _) <- lookupString $ asKeyEvent e
        return (fromMaybe xK_VoidSymbol ks, ev)
  -- Remove unused modifiers
  cleaned <- cleanMask . ev_state $ e
  return (cleaned, sym)

-- | showKey shows the next key that is pushed using Dzen (for debugging purposes)
showKey :: X ()
showKey =  do
  k <- withKB waitKey
  case showStroke <$> k of
    Nothing  -> spawn "echo \"Error\" | dzen2 -p 2"
    (Just "q") -> return ()
    (Just c) -> do
      spawn $ "echo \"" ++ c ++ "\" | dzen2 -p 2"
      showKey
  return ()

-- | getItem looks up the next keystroke in the KeyMap
getItem :: KeyMap a -> X (Maybe a)
getItem m = waitKey >>= \k -> return $ M.lookup k m

-- | waitItem waits until a matched keystroke is found and looks that up
waitItem :: KeyMap a -> X a
waitItem m = do
  x <- getItem m
  case x of
    Just a  -> return a
    Nothing -> waitItem m

-- | withKB performs an X-action in the context of having the keyboard grabbed
withKB :: X a -> X (Maybe a)
withKB x = do
  XConf { display = dpy, theRoot = win } <- ask
  status <- io $ grabKeyboard dpy win False grabModeAsync grabModeAsync currentTime
  if status == grabSuccess then
    do out <- userCode x
       io $ ungrabKeyboard dpy currentTime
       return out
    else
       return Nothing
