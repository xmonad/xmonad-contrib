{-# LANGUAGE CPP, FlexibleInstances, MultiParamTypeClasses, PatternGuards #-}
#ifdef TESTING
{-# OPTIONS_GHC -Wno-duplicate-exports #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.LimitWindows
-- Description :  A layout modifier that limits the number of windows that can be shown.
-- Copyright   :  (c) 2009 Adam Vogt
--                (c) 2009 Max Rabkin -- wrote limitSelect
-- License     :  BSD-style (see xmonad/LICENSE)
--
-- Maintainer  :  vogt.adam@gmail.com
-- Stability   :  unstable
-- Portability :  unportable
--
-- A layout modifier that limits the number of windows that can be shown.
-- See "XMonad.Layout.Minimize" for manually setting hidden windows.
--
-----------------------------------------------------------------------------

module XMonad.Layout.LimitWindows (
    -- * Usage
    -- $usage

    -- * Layout Modifiers
    limitWindows,limitSlice,limitSelect,

    -- * Change the number of windows
    increaseLimit,decreaseLimit,setLimit,

#ifdef TESTING
    -- * For tests
    select,update,Selection(..),updateAndSelect,
#endif

    -- * Types
    LimitWindows, Selection,
    ) where

import XMonad
import XMonad.Layout.LayoutModifier
import XMonad.Prelude (fromJust, guard, (<=<))
import qualified XMonad.StackSet as W

-- $usage
-- To use this module, add the following import to @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Layout.LimitWindows
--
-- > myLayout = limitWindows 6 $ Tall 1 0.03 0.5 ||| Full ||| RandomOtherLayout...
-- > main = xmonad def { layoutHook = myLayout }
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

-- | Only display the first @m@ windows and @r@ others.
-- The @IncMasterN@ message will change @m@, as well as passing it onto the
-- underlying layout.
limitSelect :: Int -> Int -> l a -> ModifiedLayout Selection l a
limitSelect m r = ModifiedLayout Sel{ nMaster=m, start=m, nRest=r }

data LimitWindows a = LimitWindows SliceStyle Int deriving (Read,Show)

data SliceStyle = FirstN | Slice deriving (Read,Show)

newtype LimitChange = LimitChange { unLC :: Int -> Int }

instance Message LimitChange

instance LayoutModifier LimitWindows a where
     pureMess (LimitWindows s n) =
        fmap (LimitWindows s) . pos <=< (`app` n) . unLC <=< fromMessage
      where pos x   = guard (x>=1)     >> return x
            app f x = guard (f x /= x) >>  return (f x)

     modifyLayout (LimitWindows style n) ws =
        runLayout ws { W.stack = f n <$> W.stack ws }
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

data Selection a = Sel { nMaster :: Int, start :: Int, nRest :: Int }
    deriving (Read, Show, Eq)

instance LayoutModifier Selection a where
    modifyLayout s w =
        runLayout (w { W.stack = updateAndSelect s <$> W.stack w })

    pureModifier sel _ stk wins = (wins, update sel <$> stk)

    pureMess sel m
        | Just f <- unLC <$> fromMessage m =
            Just $ sel { nRest = max 0 (f (nMaster sel + nRest sel) - nMaster sel) }
        | Just (IncMasterN n) <- fromMessage m =
            Just $ sel { nMaster = max 0 (nMaster sel + n) }
        | otherwise =
            Nothing

select :: Selection l -> W.Stack a -> W.Stack a
select s stk
    | lups < nMaster s
        = stk { W.down=take (nMaster s - lups - 1) downs ++
                    (take (nRest s) . drop (start s - lups - 1) $ downs) }
    | otherwise
        = stk { W.up=reverse (take (nMaster s) ups ++ drop (start s) ups),
                W.down=take (nRest s - (lups - start s) - 1) downs }
    where
        downs = W.down stk
        ups = reverse $ W.up stk
        lups = length ups

updateStart :: Selection l -> W.Stack a -> Int
updateStart s stk
    | lups < nMaster s  -- the focussed window is in the master pane
        = start s `min` (lups + ldown - nRest s + 1) `max` nMaster s
    | otherwise
        = start s `min` lups
                  `max` (lups - nRest s + 1)
                  `min` (lups + ldown - nRest s + 1)
                  `max` nMaster s
    where
        lups = length $ W.up stk
        ldown = length $ W.down stk

update :: Selection l -> W.Stack a -> Selection a
update sel stk = sel { start=updateStart sel stk }

updateAndSelect :: Selection l -> W.Stack a -> W.Stack a
updateAndSelect sel stk = select (update sel stk) stk
