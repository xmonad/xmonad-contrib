--------------------------------------------------------------------
-- |
-- Module      : XMonad.Util.CustomKeys
-- Description : Configure key bindings.
-- Copyright   : (c) 2007 Valery V. Vorotyntsev
-- License     : BSD3-style (see LICENSE)
--
-- Customized key bindings.
--
-- See also "XMonad.Util.EZConfig" in xmonad-contrib.
--------------------------------------------------------------------

module XMonad.Util.CustomKeys (
                              -- * Usage
                              -- $usage
                               customKeys
                              , customKeysFrom
                              ) where

import XMonad
import XMonad.Prelude ((<&>))
import Control.Monad.Reader

import qualified Data.Map as M

-- $usage
--
-- In @~\/.xmonad\/xmonad.hs@ add:
--
-- > import XMonad.Util.CustomKeys
--
-- Set key bindings with 'customKeys':
--
-- > main = xmonad def { keys = customKeys delkeys inskeys }
-- >     where
-- >       delkeys :: XConfig l -> [(KeyMask, KeySym)]
-- >       delkeys XConfig {modMask = modm} =
-- >           [ (modm .|. shiftMask, xK_Return) -- > terminal
-- >           , (modm .|. shiftMask, xK_c)      -- > close the focused window
-- >           ]
-- >           ++
-- >           [ (modm .|. m, k) | m <- [0, shiftMask], k <- [xK_w, xK_e, xK_r] ]
-- >
-- >       inskeys :: XConfig l -> [((KeyMask, KeySym), X ())]
-- >       inskeys conf@(XConfig {modMask = modm}) =
-- >           [ ((mod1Mask,             xK_F2  ), spawn $ terminal conf) -- mod1-f2 %! Run a terminal emulator
-- >           , ((modm,                 xK_Delete), kill) -- %! Close the focused window
-- >           , ((modm .|. controlMask, xK_F11 ), spawn "xscreensaver-command -lock")
-- >           , ((mod1Mask,             xK_Down), spawn "amixer set Master 1-")
-- >           , ((mod1Mask,             xK_Up  ), spawn "amixer set Master 1+")
-- >           ]

-- | Customize 'XMonad.Config.def' -- delete needless
-- shortcuts and insert those you will use.
customKeys :: (XConfig Layout -> [(KeyMask, KeySym)]) -- ^ shortcuts to delete
           -> (XConfig Layout -> [((KeyMask, KeySym), X ())]) -- ^ key bindings to insert
           -> XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
customKeys = customKeysFrom def

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
customize conf ds is = asks (keys conf) >>= delete ds >>= insert is

delete :: (MonadReader r m, Ord a) => (r -> [a]) -> M.Map a b -> m (M.Map a b)
delete dels kmap = asks dels <&> foldr M.delete kmap

insert :: (MonadReader r m, Ord a) =>
          (r -> [(a, b)]) -> M.Map a b -> m (M.Map a b)
insert ins kmap = asks ins <&> foldr (uncurry M.insert) kmap
