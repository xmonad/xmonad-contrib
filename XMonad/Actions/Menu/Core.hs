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

import           Control.Arrow     ((&&&))
import           Control.Monad     (void)
import qualified Data.Map          as M
import           XMonad
import           XMonad.Keys.Core  (KeyStroke, withKB, waitItem)

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
