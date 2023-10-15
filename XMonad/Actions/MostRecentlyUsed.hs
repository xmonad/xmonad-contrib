{-# LANGUAGE NamedFieldPuns, GeneralizedNewtypeDeriving #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Actions.MostRecentlyUsed
-- Description :  Tab through windows by recency of use.
-- Copyright   :  (c) 2022 L. S. Leary
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  @LSLeary (on github)
-- Stability   :  unstable
-- Portability :  unportable
--
-- Based on the Alt+Tab behaviour common outside of xmonad.
--
-----------------------------------------------------------------------------

-- --< Imports & Exports >-- {{{

module XMonad.Actions.MostRecentlyUsed (

  -- * Usage
  -- $usage

  -- * Interface
  configureMRU,
  mostRecentlyUsed,
  withMostRecentlyUsed,
  Location(..),

  ) where

-- base
import Data.Maybe (fromMaybe)
import Data.List.NonEmpty (NonEmpty(..), nonEmpty)
import Data.Monoid (All(..), Any)
import Data.Foldable (for_)
import Data.Functor (($>))
import Data.IORef (newIORef, readIORef, writeIORef, modifyIORef)
import Control.Applicative (liftA2)
import Control.Monad (when, unless, join)
import Control.Monad.IO.Class (MonadIO)

-- mtl
import Control.Monad.Trans (lift)
import Control.Monad.State (get, put, gets)

-- containers
import qualified Data.Map.Strict as M

-- xmonad
import XMonad
  ( Window, KeySym, keyPress, io
  , Event (DestroyWindowEvent, UnmapEvent, ev_send_event, ev_window)
  )
import XMonad.Core
  ( X, XConfig(..), windowset, WorkspaceId, ScreenId
  , ExtensionClass(..), StateExtension(..)
  , waitingUnmap
  )
import XMonad.Operations (screenWorkspace)
import qualified XMonad.StackSet as W

-- xmonad-contrib
import qualified XMonad.Util.ExtensibleConf  as XC
import qualified XMonad.Util.ExtensibleState as XS
import XMonad.Util.PureX
  (handlingRefresh, curScreenId, curTag, greedyView, view, peek, focusWindow)
import XMonad.Util.History (History, origin, event, erase, ledger)
import XMonad.Actions.Repeatable (repeatableSt)
import XMonad.Prelude (Stream (..), cycleS)

-- }}}

-- --< Core Data Types: WindowHistory & Location >-- {{{

data WindowHistory = WinHist
  { busy :: !Bool
  , hist :: !(History Window Location)
  } deriving (Show, Read)

instance ExtensionClass WindowHistory where
  initialValue = WinHist
    { busy = False
    , hist = origin
    }
  extensionType = PersistentExtension

data Location = Location
  { workspace :: !WorkspaceId
  , screen    :: !ScreenId
  } deriving (Show, Read, Eq, Ord)

-- }}}

-- --< Interface >-- {{{

-- $usage
--
-- 'configureMRU' must be applied to your config in order for 'mostRecentlyUsed'
-- to work.
--
-- > main :: IO ()
-- > main = xmonad . configureMRU . ... $ def
-- >   { ...
-- >   }
--
-- Once that's done, it can be used normally in keybinds:
--
-- > , ((mod1Mask, xK_Tab), mostRecentlyUsed [xK_Alt_L, xK_Alt_R] xK_Tab)
--
-- N.B.: This example assumes that 'mod1Mask' corresponds to alt, which is not
-- always the case, depending on how your system is configured.

-- | Configure xmonad to support 'mostRecentlyUsed'.
configureMRU :: XConfig l -> XConfig l
configureMRU = XC.once f (MRU ()) where
  f cnf = cnf
    { logHook         = logHook         cnf <> logWinHist
    , handleEventHook = handleEventHook cnf <> winHistEH
    }
newtype MRU = MRU () deriving Semigroup

-- | An action to browse through the history of focused windows, taking
--   another step back with each tap of the key.
mostRecentlyUsed
  :: [KeySym] -- ^ The 'KeySym's corresponding to the modifier to which the
              --   action is bound.
  -> KeySym   -- ^ The 'KeySym' corresponding to the key to which the action
              --   is bound.
  -> X ()
mostRecentlyUsed mods key = do
  (toUndo, undo) <- undoer
  let undoably curThing withThing thing = curThing >>= \cur ->
        when (cur /= thing) $ withThing thing >> toUndo (withThing cur)
  withMostRecentlyUsed mods key $ \win Location{workspace,screen} ->
    handlingRefresh $ do
      undo
      undoably curScreenId viewScreen screen
      undoably curTag      greedyView workspace
      mi <- gets (W.findTag win . windowset)
      for_ mi $ \i -> do
        undoably curTag greedyView i
        mfw <- peek
        for_ mfw $ \fw -> do
          undoably (pure fw) focusWindow win
  where
    undoer :: (MonadIO m, Monoid a) => m (m a -> m (), m a)
    undoer = do
      ref <- io . newIORef $ pure mempty
      let toUndo = io . modifyIORef ref . liftA2 (<>)
          undo   = join (io $ readIORef ref)
                <* io (writeIORef ref $ pure mempty)
      pure (toUndo, undo)
    viewScreen :: ScreenId -> X Any
    viewScreen scr = screenWorkspace scr >>= foldMap view

-- | A version of 'mostRecentlyUsed' that allows you to customise exactly what
--   is done with each window you tab through (the default being to visit its
--   previous 'Location' and give it focus).
withMostRecentlyUsed
  :: [KeySym]                     -- ^ The 'KeySym's corresponding to the
                                  --   modifier to which the action is bound.
  -> KeySym                       -- ^ The 'KeySym' corresponding to the key to
                                  --   which the action is bound.
  -> (Window -> Location -> X ()) -- ^ The function applied to each window.
  -> X ()
withMostRecentlyUsed mods tab preview = do
  wh@WinHist{busy,hist} <- XS.get
  unless busy $ do
    XS.put wh{ busy = True }

    for_ (nonEmpty $ ledger hist) $ \ne -> do
      mfw <- gets (W.peek . windowset)
      let iSt = case cycleS ne of
            (w, _) :~ s | mfw == Just w -> s
            s                           -> s
      repeatableSt iSt mods tab $ \t s ->
        when (t == keyPress && s == tab) (pop >>= lift . uncurry preview)

    XS.modify $ \ws@WinHist{} -> ws{ busy = False }
    logWinHist
  where
    pop = do
      h :~ t <- get
      put t $> h

-- }}}

-- --< Raw Config >-- {{{

logWinHist :: X ()
logWinHist = do
  wh@WinHist{busy,hist} <- XS.get
  unless busy $ do
    cs <- gets (W.current . windowset)
    let cws = W.workspace cs
    for_ (W.stack cws) $ \st -> do
      let location = Location{ workspace = W.tag cws, screen = W.screen cs }
      XS.put wh{ hist = event (W.focus st) location hist }

winHistEH :: Event -> X All
winHistEH ev = All True <$ case ev of
  UnmapEvent{ ev_send_event = synth, ev_window = w } -> do
    e <- gets (fromMaybe 0 . M.lookup w . waitingUnmap)
    when (synth || e == 0) (collect w)
  DestroyWindowEvent{                ev_window = w } -> collect w
  _                                                  -> pure ()
  where collect w = XS.modify $ \wh@WinHist{hist} -> wh{ hist = erase w hist }

-- }}}
