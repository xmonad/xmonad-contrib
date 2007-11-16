--------------------------------------------------------------------
-- |
-- Module     : XMonad.Util.CustomKeys
-- Copyright  : (c) 2007 Valery V. Vorotyntsev
-- License    : BSD3-style (see LICENSE)
--
-- Maintainer : Valery V. Vorotynsev <valery.vv@gmail.com>
--
-- Customized key bindings.
--
-- (See also "XMonad.Util.EZConfig" in xmonad-contrib.)
--------------------------------------------------------------------

module XMonad.Util.CustomKeys (
                              -- * Usage
                              -- $usage
                               customKeys
                              , customKeysFrom
                              ) where

import XMonad
import Graphics.X11.Xlib

import Control.Monad.Reader
import qualified Data.Map as M

-- $usage
--
-- 1. In @~\/.xmonad\/xmonad.hs@ add:
--
-- > import XMonad.Util.CustomKeys
--
-- 2. Set key bindings with 'customKeys':
--
-- > main = xmonad defaultConfig { keys = customKeys delkeys inskeys }
-- >     where
-- >       delkeys :: XConfig l -> [(KeyMask, KeySym)]
-- >       delkeys XConfig {modMask = modm} =
-- >           -- we're preferring Futurama to Xinerama here
-- >           [ (modm .|. m, k) | (m, k) <- zip [0, shiftMask] [xK_w, xK_e, xK_r] ]
-- >
-- >       inskeys :: XConfig l -> [((KeyMask, KeySym), X ())]
-- >       inskeys conf@(XConfig {modMask = modm}) =
-- >           [ ((mod1Mask,             xK_F2  ), spawn $ terminal conf)
-- >           , ((modm .|. controlMask, xK_F11 ), spawn "xscreensaver-command -lock")
-- >           , ((mod1Mask,             xK_Down), spawn "amixer set Master 1-")
-- >           , ((mod1Mask,             xK_Up  ), spawn "amixer set Master 1+")
-- >           ]
--
-- 0 (/hidden feature/). You can always replace bindings map
--   entirely. No need to import "CustomKeys" this time:
--
-- > import XMonad
-- > import Graphics.X11.Xlib
-- > import System.Exit
-- > import qualified Data.Map as M
-- >
-- > main = xmonad defaultConfig {
-- >          keys = \_ -> M.fromList [
-- >                  -- Let me out of here! I want my KDE back! Help! Help!
-- >                  ( (0, xK_Escape), io (exitWith ExitSuccess) ) ] }

-- | Customize 'XMonad.Config.defaultConfig' -- delete needless
-- shortcuts and insert those you will use.
customKeys :: (XConfig Layout -> [(KeyMask, KeySym)]) -- ^ shortcuts to delete
           -> (XConfig Layout -> [((KeyMask, KeySym), X ())]) -- ^ key bindings to insert
           -> XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
customKeys = customKeysFrom defaultConfig

-- | General variant of 'customKeys': customize key bindings of
-- third-party configuration.
customKeysFrom :: XConfig l -- ^ original configuration
               -> (XConfig Layout -> [(KeyMask, KeySym)]) -- ^ shortcuts to delete
               -> (XConfig Layout -> [((KeyMask, KeySym), X ())]) -- ^ key bindings to insert
               -> XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
customKeysFrom conf = (runReader .) . customize conf

customize :: XConfig l
          -> (XConfig Layout -> [(KeyMask, KeySym)])
          -> (XConfig Layout -> [((KeyMask, KeySym), X ())])
          -> Reader (XConfig Layout) (M.Map (KeyMask, KeySym) (X ()))
customize conf ds is = Reader (keys conf) >>= delete ds >>= insert is

delete :: (MonadReader r m, Ord a) => (r -> [a]) -> M.Map a b -> m (M.Map a b)
delete dels kmap = asks dels >>= return . foldr M.delete kmap

insert :: (MonadReader r m, Ord a) =>
          (r -> [(a, b)]) -> M.Map a b -> m (M.Map a b)
insert ins kmap = asks ins >>= return . foldr (uncurry M.insert) kmap
