--------------------------------------------------------------------
-- |
-- Module     : XMonad.Config.CustomKeys
-- Copyright  : (c) 2007 Valery V. Vorotyntsev
-- License    : BSD3-style (see LICENSE)
--
-- Maintainer : Valery V. Vorotynsev <valery.vv@gmail.com>
--
-- Customized key bindings.
--
-- (See also "XMonad.Util.EZConfig" in XMonadContrib.)
--------------------------------------------------------------------

module XMonad.Config.CustomKeys (
                                 -- * Usage
                                 -- $usage
                                 customKeys
                                ) where

import XMonad
import Graphics.X11.Xlib

import Control.Monad.Reader
import qualified Data.Map as M

-- $usage
--
-- 1. In @~\/.xmonad\/xmonad.hs@ add:
--
-- > import XMonad.Config.CustomKeys
--
-- 2. Set key bindings with 'customKeys':
--
-- > main = xmonad defaultConfig { keys = customKeys delkeys inskeys }
-- >     where
-- >       delkeys :: XConfig l -> [(KeyMask, KeySym)]
-- >       delkeys XConfig {modMask = modm} =
-- >           -- we're preferring Futurama to Xinerama here
-- >           [ (modm .|. m, k) | (m, k) <- zip [0, shiftMas] [xK_w, xK_e, xK_r] ]
-- >
-- >       inskeys :: XConfig l -> [((KeyMask, KeySym), X ())]
-- >       inskeys conf@(XConfig {modMask = modm}) =
-- >           [ ((mod1Mask,             xK_F2  ), spawn $ terminal conf)
-- >           , ((modm .|. controlMask, xK_F11 ), spawn "xscreensaver-command -lock")
-- >           , ((mod1Mask,             xK_Down), spawn "amixer set Master 1-")
-- >           , ((mod1Mask,             xK_Up  ), spawn "amixer set Master 1+")
-- >           ]

-- | XXX comment me (no tautology please)
customKeys :: (XConfig Layout -> [(KeyMask, KeySym)]) -- ^ unused shortcuts
           -> (XConfig Layout -> [((KeyMask, KeySym), X ())]) -- ^ new bindings
           -> XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
customKeys = (runReader .) . customize

customize :: (XConfig Layout -> [(KeyMask, KeySym)])
          -> (XConfig Layout -> [((KeyMask, KeySym), X ())])
          -> Reader (XConfig Layout) (M.Map (KeyMask, KeySym) (X ()))
customize ds is = Reader (keys defaultConfig) >>= delete ds >>= insert is

delete :: (MonadReader r m, Ord a) => (r -> [a]) -> M.Map a b -> m (M.Map a b)
delete dels kmap = asks dels >>= return . foldr M.delete kmap

insert :: (MonadReader r m, Ord a) =>
          (r -> [(a, b)]) -> M.Map a b -> m (M.Map a b)
insert ins kmap = asks ins >>= return . foldr (uncurry M.insert) kmap
