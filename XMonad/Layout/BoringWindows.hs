{-# OPTIONS_GHC -fglasgow-exts #-} -- For deriving Data/Typeable

-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.BoringWindows
-- Copyright   :  (c) 2008  David Roundy <droundy@darcs.net>
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  none
-- Stability   :  unstable
-- Portability :  unportable
--
-- BoringWindows is an extension to allow windows to be marked boring
--
-----------------------------------------------------------------------------

module XMonad.Layout.BoringWindows (
                                   -- * Usage
                                   -- $usage
                                   boringWindows,
                                   markBoring, clearBoring,
                                   focusUp, focusDown
                                  ) where

import XMonad hiding (Point)
import qualified XMonad.StackSet as W
import XMonad.Layout.LayoutModifier
import XMonad.Util.Invisible

data BoringMessage = FocusUp | FocusDown | IsBoring Window | ClearBoring
                       deriving ( Read, Show, Typeable )
instance Message BoringMessage

markBoring, clearBoring, focusUp, focusDown :: X ()
markBoring = withFocused (sendMessage . IsBoring)
clearBoring = sendMessage ClearBoring
focusUp = sendMessage FocusUp
focusDown = sendMessage FocusDown

data BoringWindows a = BoringWindows (Invisible [] a) deriving ( Show, Read, Typeable )

boringWindows :: (LayoutClass l a, Eq a) => l a -> ModifiedLayout BoringWindows l a
boringWindows = ModifiedLayout (BoringWindows (I []))

instance LayoutModifier BoringWindows Window where
    handleMessOrMaybeModifyIt (BoringWindows (I bs)) m
        | Just (IsBoring b) <- fromMessage m = return $ Just $ Left $ BoringWindows (I (b:bs))
        | Just ClearBoring <- fromMessage m = return $ Just $ Left $ BoringWindows (I [])
        | Just FocusUp <- fromMessage m = do windows $ W.modify' $ focusUp'
                                             return Nothing
        | Just FocusDown <- fromMessage m =
                            do windows $ W.modify' (reverseStack . focusUp' . reverseStack)
                               return Nothing
        where focusUp' (W.Stack t ls rs)
                  | (a,l:ls') <- skipBoring ls = W.Stack l ls' (a++t:rs)
                  | otherwise = case skipBoring (reverse (t:rs)++ls) of
                                (a,x:xs) -> W.Stack x xs a
                                _ -> W.Stack t ls rs
              skipBoring [] = ([],[])
              skipBoring (x:xs) | x `elem` bs = case skipBoring xs of
                                                (a,b) -> (x:a,b)
                                | otherwise = ([],x:xs)
    handleMessOrMaybeModifyIt _ _ = return Nothing

-- | reverse a stack: up becomes down and down becomes up.
reverseStack :: W.Stack a -> W.Stack a
reverseStack (W.Stack t ls rs) = W.Stack t rs ls
