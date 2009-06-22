{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, DeriveDataTypeable #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.LimitWindows
-- Copyright   :  (c) 2009 Adam Vogt
-- License     :  BSD-style (see xmonad/LICENSE)
--
-- Maintainer  :  vogt.adam@gmail.com
-- Stability   :  unstable
-- Portability :  unportable
--
-- A layout modifier that limits the number of windows that can be shown.
--
-----------------------------------------------------------------------------

module XMonad.Layout.LimitWindows (
    -- * Usage
    -- $usage

    -- Layout Modifiers
    limitWindows,limitSlice,

    -- Change the number of windows
    increaseLimit,decreaseLimit,setLimit
    ) where

import XMonad.Layout.LayoutModifier
import XMonad
import qualified XMonad.StackSet as W
import Control.Monad((<=<),guard)
import Data.Maybe(fromJust)

-- $usage
-- To use this module, add the following import to @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Layout.LimitWindows
--
-- > myLayout = limitWindows 6 $ Tall 1 0.03 0.5 ||| Full ||| RandomOtherLayout...
-- > main = xmonad defaultConfig { layoutHook = myLayout }
--
-- You may also be interested in dynamically changing the number dynamically,
-- by binding keys to the 'increaseLimit', 'decreaseLimit', or 'setLimit'
-- actions.
--
-- For detailed instructions on editing your key bindings, see
-- "XMonad.Doc.Extending#Editing_key_bindings".
--
-- See also 'XMonad.Layout.BoringWindows.boringAuto' for keybindings that skip
-- the hidden windows.

increaseLimit :: X ()
increaseLimit = sendMessage $ LimitChange succ

decreaseLimit :: X ()
decreaseLimit = sendMessage . LimitChange $ max 1 . pred

setLimit :: Int -> X ()
setLimit tgt = sendMessage . LimitChange $ const tgt

-- | Only display the first @n@ windows.
limitWindows :: Int -> l a -> ModifiedLayout LimitWindows l a
limitWindows n = ModifiedLayout (LimitWindows FirstN n)

-- | Only display @n@ windows around the focused window. This makes sense with
-- layouts that arrange windows linearily, like 'XMonad.Layout.Layout.Accordion'.
limitSlice :: Int -> l a -> ModifiedLayout LimitWindows l a
limitSlice n = ModifiedLayout (LimitWindows Slice n)

data LimitWindows a = LimitWindows SliceStyle Int deriving (Read,Show)

data SliceStyle = FirstN | Slice deriving (Read,Show)

data LimitChange = LimitChange { unLC :: (Int -> Int) } deriving (Typeable)

instance Message LimitChange

instance LayoutModifier LimitWindows a where
     pureMess (LimitWindows s n) =
        fmap (LimitWindows s) . pos <=< (`app` n) . unLC <=< fromMessage
      where pos x   = guard (x>=1)     >> return x
            app f x = guard (f x /= x) >>  return (f x)

     modifyLayout (LimitWindows style n) ws r =
        runLayout ws { W.stack = f n `fmap` W.stack ws } r
      where f = case style of
                    FirstN -> firstN
                    Slice -> slice

firstN ::  Int -> W.Stack a -> W.Stack a
firstN n st = upfocus $ fromJust $ W.differentiate $ take (max 1 n) $ W.integrate st
    where upfocus = foldr (.) id $ replicate (length (W.up st)) W.focusDown'

-- | A non-wrapping, fixed-size slice of a stack around the focused element
slice ::  Int -> W.Stack t -> W.Stack t
slice n (W.Stack f u d) =
        W.Stack f (take (nu + unusedD) u)
                  (take (nd + unusedU) d)
    where unusedD = max 0 $ nd - length d
          unusedU = max 0 $ nu - length u
          nd = div (n - 1) 2
          nu = uncurry (+) $ divMod (n - 1) 2
