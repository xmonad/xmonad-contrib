{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP          #-}
{-# LANGUAGE LambdaCase   #-}
--------------------------------------------------------------------
-- |
-- Module      :  XMonad.Prelude
-- Description :  Utility functions and re-exports.
-- Copyright   :  slotThe <soliditsallgood@mailbox.org>
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  slotThe <soliditsallgood@mailbox.org>
--
-- Utility functions and re-exports for a more ergonomic developing
-- experience.  Users themselves will not find much use here.
--
--------------------------------------------------------------------
module XMonad.Prelude (
    module Exports,
    fi,
    chunksOf,
    (.:),
    (!?),
    NonEmpty((:|)),
    notEmpty,
    safeGetWindowAttributes,
    keyToString,
    keymaskToString,
    cleanKeyMask,
) where

import Foreign (alloca, peek)
import XMonad

import Control.Applicative as Exports
import Control.Monad       as Exports
import Data.Bool           as Exports
import Data.Char           as Exports
import Data.Foldable       as Exports
import Data.Function       as Exports
import Data.Functor        as Exports
import Data.List           as Exports
import Data.Maybe          as Exports
import Data.Monoid         as Exports
import Data.Traversable    as Exports

import Data.Bifunctor (bimap)
import Data.Bits
import Data.List.NonEmpty (NonEmpty ((:|)))
import GHC.Stack

-- | Short for 'fromIntegral'.
fi :: (Integral a, Num b) => a -> b
fi = fromIntegral

-- | Given a maximum length, splits a list into sublists
--
-- >>> chunksOf 5 (take 30 $ repeat 'a')
-- ["aaaaa","aaaaa","aaaaa","aaaaa","aaaaa","aaaaa"]
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf i xs = chunk : chunksOf i rest
  where !(chunk, rest) = splitAt i xs

-- | Safe version of '(!!)'.
(!?) :: [a] -> Int -> Maybe a
(!?) xs n | n < 0 = Nothing
          | otherwise = listToMaybe $ drop n xs

-- | Multivariant composition.
--
-- > f .: g ≡ (f .) . g ≡ \c d -> f (g c d)
(.:) :: (a -> b) -> (c -> d -> a) -> c -> d -> b
(.:) = (.) . (.)

-- | 'Data.List.NonEmpty.fromList' with a better error message. Useful to
-- silence GHC's Pattern match(es) are non-exhaustive warning in places where
-- the programmer knows it's always non-empty, but it's infeasible to express
-- that in the type system.
notEmpty :: HasCallStack => [a] -> NonEmpty a
notEmpty [] = error "unexpected empty list"
notEmpty (x:xs) = x :| xs

-- | A safe version of 'Graphics.X11.Extras.getWindowAttributes'.
safeGetWindowAttributes :: Window -> X (Maybe WindowAttributes)
safeGetWindowAttributes w = withDisplay $ \dpy -> io . alloca $ \p ->
  xGetWindowAttributes dpy w p >>= \case
    0 -> pure Nothing
    _ -> Just <$> peek p

-- | Convert a modifier mask into a useful string.
keymaskToString :: KeyMask -- ^ Num lock mask
                -> KeyMask -- ^ Modifier mask
                -> String
keymaskToString numLockMask msk =
  unwords . reverse . fst . foldr go ([], msk) $ masks
 where
  masks :: [(KeyMask, String)]
  masks = map (\m -> (m, show m))
              [0 .. toEnum (finiteBitSize msk - 1)]
       ++ [ (numLockMask, "num-" )
          , (lockMask,    "lock-")
          , (controlMask, "C-"   )
          , (shiftMask,   "S-"   )
          , (mod5Mask,    "M5-"  )
          , (mod4Mask,    "M4-"  )
          , (mod3Mask,    "M3-"  )
          , (mod2Mask,    "M2-"  )
          , (mod1Mask,    "M1-"  )
          ]

  go :: (KeyMask, String) -> ([String], KeyMask) -> ([String], KeyMask)
  go (m, s) a@(ss, v)
    | v == 0       = a
    | v .&. m == m = (s : ss, v .&. complement m)
    | otherwise    = a

-- | Convert a full key combination; i.e., a 'KeyMask' and 'KeySym'
-- pair, into a string.
keyToString :: (KeyMask, KeySym) -> [Char]
keyToString = uncurry (++) . bimap (keymaskToString 0) keysymToString

-- | Strip numlock, capslock, mouse buttons and XKB group from a 'KeyMask',
-- leaving only modifier keys like Shift, Control, Super, Hyper in the mask
-- (hence the \"Key\" in \"cleanKeyMask\").
--
-- Core's 'cleanMask' only strips the first two because key events from
-- passive grabs (key bindings) are stripped of mouse buttons and XKB group by
-- the X server already for compatibility reasons. For more info, see:
-- <https://www.x.org/releases/X11R7.7/doc/kbproto/xkbproto.html#Delivering_a_Key_or_Button_Event_to_a_Client>
cleanKeyMask :: X (KeyMask -> KeyMask)
#if MIN_VERSION_xmonad(0, 17, 1)
cleanKeyMask = cleanKeyMask' <$> join (asks (stripModMask . config))
#else
cleanKeyMask = cleanKeyMask' . (lockMask .|.) <$> gets numberlockMask
#endif

cleanKeyMask' :: KeyMask -> KeyMask -> KeyMask
cleanKeyMask' stripMask mask =
    mask .&. complement stripMask .&. (button1Mask - 1)
