{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternGuards, FlexibleContexts, FlexibleInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.BoringWindows
-- Description :  Mark windows as boring.
-- Copyright   :  (c) 2008  David Roundy <droundy@darcs.net>
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  Adam Vogt <vogt.adam@gmail.com>
-- Stability   :  unstable
-- Portability :  unportable
--
-- BoringWindows is an extension to allow windows to be marked boring
--
-----------------------------------------------------------------------------

module XMonad.Layout.BoringWindows (
                                   -- * Usage
                                   -- $usage
                                   boringWindows, boringAuto,
                                   markBoring, markBoringEverywhere,
                                   clearBoring, focusUp, focusDown,
                                   focusMaster, swapUp, swapDown,
                                   siftUp, siftDown,

                                   UpdateBoring(UpdateBoring),
                                   BoringMessage(Replace,Merge),
                                   BoringWindows()

                                   -- * Tips
                                   -- ** variant of 'Full'
                                   -- $simplest
                                  ) where

import XMonad.Layout.LayoutModifier(ModifiedLayout(..),
                                    LayoutModifier(handleMessOrMaybeModifyIt, redoLayout))
import XMonad(LayoutClass, Message, X, fromMessage,
              broadcastMessage, sendMessage, windows, withFocused, Window)
import XMonad.Prelude
import XMonad.Util.Stack (reverseS)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import qualified XMonad.StackSet as W

-- $usage
-- You can use this module with the following in your
-- @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Layout.BoringWindows
--
-- Then edit your @layoutHook@ by adding the layout modifier:
--
-- > myLayout = boringWindows (Full ||| etc..)
-- > main = xmonad def { layoutHook = myLayout }
--
-- Then to your keybindings, add:
--
-- > , ((modm, xK_j), focusUp)
-- > , ((modm, xK_k), focusDown)
-- > , ((modm, xK_m), focusMaster)
--
-- For more detailed instructions on editing the layoutHook see:
--
-- "XMonad.Doc.Extending#Editing_the_layout_hook"


data BoringMessage = FocusUp | FocusDown | FocusMaster | IsBoring Window | ClearBoring
                     | Replace String [Window]
                     | Merge String [Window]
                     | SwapUp
                     | SwapDown
                     | SiftUp
                     | SiftDown
                     deriving ( Read, Show )

instance Message BoringMessage

-- | UpdateBoring is sent before attempting to view another boring window, so
-- that layouts have a chance to mark boring windows.
data UpdateBoring = UpdateBoring
instance Message UpdateBoring

markBoring, clearBoring, focusUp, focusDown, focusMaster, swapUp, swapDown, siftUp, siftDown :: X ()
markBoring = withFocused (sendMessage . IsBoring)
clearBoring = sendMessage ClearBoring
focusUp = sendMessage UpdateBoring >> sendMessage FocusUp
focusDown = sendMessage UpdateBoring >> sendMessage FocusDown
focusMaster = sendMessage UpdateBoring >> sendMessage FocusMaster
swapUp = sendMessage UpdateBoring >> sendMessage SwapUp
swapDown = sendMessage UpdateBoring >> sendMessage SwapDown
siftUp = sendMessage UpdateBoring >> sendMessage SiftUp
siftDown = sendMessage UpdateBoring >> sendMessage SiftDown

-- | Mark current focused window boring for all layouts.
-- This is useful in combination with the 'XMonad.Actions.CopyWindow' module.
markBoringEverywhere :: X ()
markBoringEverywhere = withFocused (broadcastMessage . IsBoring)

data BoringWindows a = BoringWindows
    { namedBoring :: M.Map String [a] -- ^ store borings with a specific source
    , chosenBoring :: [a]             -- ^ user-chosen borings
    , hiddenBoring :: Maybe [a]       -- ^ maybe mark hidden windows
    } deriving (Show,Read)

boringWindows :: (LayoutClass l a, Eq a) => l a -> ModifiedLayout BoringWindows l a
boringWindows = ModifiedLayout (BoringWindows M.empty [] Nothing)

-- | Mark windows that are not given rectangles as boring
boringAuto :: (LayoutClass l a, Eq a) => l a -> ModifiedLayout BoringWindows l a
boringAuto = ModifiedLayout (BoringWindows M.empty [] (Just []))

instance LayoutModifier BoringWindows Window where
    redoLayout b@BoringWindows{ hiddenBoring = bs } _r mst arrs = do
        let bs' = W.integrate' mst \\ map fst arrs
        return (arrs, Just $ b { hiddenBoring = bs' <$ bs } )

    handleMessOrMaybeModifyIt bst@(BoringWindows nbs cbs lbs) m
        | Just (Replace k ws) <- fromMessage m
        , Just ws /= M.lookup k nbs =
            let nnb = if null ws then M.delete k nbs
                          else M.insert k ws nbs
            in rjl bst { namedBoring = nnb }
        | Just (Merge k ws) <- fromMessage m
        , maybe True (not . null . (ws \\)) (M.lookup k nbs) =
            rjl bst { namedBoring = M.insertWith union k ws nbs }
        | Just (IsBoring w) <- fromMessage m , w `notElem` cbs =
            rjl bst { chosenBoring = w:cbs }
        | Just ClearBoring <- fromMessage m, not (null cbs) =
            rjl bst { namedBoring = M.empty, chosenBoring = []}
        | Just FocusUp <- fromMessage m =
                            do windows $ W.modify' $ skipBoring W.focusUp'
                               return Nothing
        | Just FocusDown <- fromMessage m =
                            do windows $ W.modify' $ skipBoring W.focusDown'
                               return Nothing
        | Just FocusMaster <- fromMessage m =
                            do windows $ W.modify'
                                            $ skipBoring W.focusDown' -- wiggle focus to make sure
                                            . skipBoring W.focusUp'   -- no boring window gets the focus
                                            . focusMaster'
                               return Nothing
        | Just SwapUp <- fromMessage m =
                            do windows $ W.modify' skipBoringSwapUp
                               return Nothing
        | Just SwapDown <- fromMessage m =
                            do windows $ W.modify' (reverseS . skipBoringSwapUp . reverseS)
                               return Nothing
        | Just SiftUp <- fromMessage m =
                            do windows $ W.modify' (siftUpSkipping bs)
                               return Nothing
        | Just SiftDown <- fromMessage m =
                            do windows $ W.modify' (reverseS . siftUpSkipping bs . reverseS)
                               return Nothing
        where skipBoring = skipBoring' ((`notElem` bs) . W.focus)
              skipBoringSwapUp = skipBoring'
                                   (maybe True (`notElem` bs) . listToMaybe . W.down)
                                   swapUp'
              skipBoring' p f st = fromMaybe st
                                   $ find p
                                   $ drop 1
                                   $ take (length $ W.integrate st)
                                   $ iterate f st
              bs = concat $ cbs:maybeToList lbs ++ M.elems nbs
              rjl = return . Just . Left
    handleMessOrMaybeModifyIt _ _ = return Nothing

-- | Variant of 'focusMaster' that works on a
-- 'Stack' rather than an entire 'StackSet'.
focusMaster' :: W.Stack a -> W.Stack a
focusMaster' c@(W.Stack _ [] _) = c
focusMaster' (W.Stack t (l:ls) rs) = W.Stack x [] (xs ++ t : rs) where (x :| xs) = NE.reverse (l :| ls)

swapUp' :: W.Stack a -> W.Stack a
swapUp' (W.Stack t (l:ls) rs) = W.Stack t ls (l:rs)
swapUp' (W.Stack t []     rs) = W.Stack t (reverse rs) []

siftUpSkipping :: Eq a => [a] -> W.Stack a -> W.Stack a
siftUpSkipping bs (W.Stack t ls rs)
  | (skips, l:ls') <- spanLeft  = W.Stack t ls' (reverse skips ++ l : rs)
  | (skips, r:rs') <- spanRight = W.Stack t (rs' ++ r : ls) (reverse skips)
  | otherwise                   = W.Stack t ls rs
  where
    spanLeft  = span (`elem` bs) ls
    spanRight = span (`elem` bs) (reverse rs)

{- $simplest

An alternative to 'Full' is "XMonad.Layout.Simplest".  Less windows are
ignored by 'focusUp' and 'focusDown'. This may be helpful when you want windows
to be uninteresting by some other layout modifier (ex.
"XMonad.Layout.Minimize")

-}
