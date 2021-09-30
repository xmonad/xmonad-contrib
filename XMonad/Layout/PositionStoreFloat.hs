{-# LANGUAGE TypeSynonymInstances, MultiParamTypeClasses, PatternGuards #-}
----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.PositionStoreFloat
-- Description :  A floating layout; designed with a dual-head setup in mind.
-- Copyright   :  (c) Jan Vornberger 2009
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  jan.vornberger@informatik.uni-oldenburg.de
-- Stability   :  unstable
-- Portability :  not portable
--
-- A floating layout which has been designed with a dual-head setup
-- in mind. It makes use of "XMonad.Util.PositionStore" as well as
-- "XMonad.Hooks.PositionStoreHooks" . Since there is currently no way
-- to move or resize windows with the keyboard alone in this layout,
-- it is adviced to use it in combination with a decoration such as
-- "XMonad.Layout.NoFrillsDecoration" (to move windows) and the
-- layout modifier "XMonad.Layout.BorderResize" (to resize windows).
--
-----------------------------------------------------------------------------

module XMonad.Layout.PositionStoreFloat
    ( -- * Usage
      -- $usage
      positionStoreFloat, PositionStoreFloat
    ) where

import XMonad
import XMonad.Util.PositionStore
import qualified XMonad.StackSet as S
import XMonad.Layout.WindowArranger
import XMonad.Prelude (fromMaybe, isJust, nub, when)

-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Layout.PositionStoreFloat
-- > import XMonad.Layout.NoFrillsDecoration
-- > import XMonad.Layout.BorderResize
--
-- Then edit your @layoutHook@ by adding the PositionStoreFloat layout.
-- Below is a suggestion which uses the mentioned NoFrillsDecoration and
-- BorderResize:
--
-- > myLayouts = floatingDeco $ borderResize $ positionStoreFloat ||| etc..
-- >               where floatingDeco l = noFrillsDeco shrinkText def l
-- > main = xmonad def { layoutHook = myLayouts }
--
-- See the documentation of "XMonad.Hooks.PositionStoreHooks" on how
-- to add the support hooks.

positionStoreFloat :: PositionStoreFloat a
positionStoreFloat = PSF (Nothing, [])

newtype PositionStoreFloat a = PSF (Maybe Rectangle, [a]) deriving (Show, Read)
instance LayoutClass PositionStoreFloat Window where
    description _ = "PSF"
    doLayout (PSF (maybeChange, paintOrder)) sr (S.Stack w l r) = do
            posStore <- getPosStore
            let wrs = map (\w' -> (w', pSQ posStore w' sr)) (reverse l ++ r)
            let focused = case maybeChange of
                            Nothing -> (w, pSQ posStore w sr)
                            Just changedRect -> (w, changedRect)
            let wrs' = focused : wrs
            let paintOrder' = nub (w : paintOrder)
            when (isJust maybeChange) $
                updatePositionStore focused sr
            return (reorder wrs' paintOrder', Just $ PSF (Nothing, paintOrder'))
        where
            pSQ posStore w' sr' = fromMaybe (Rectangle 50 50 200 200)       -- should usually not happen
                                            (posStoreQuery posStore w' sr')
    pureMessage (PSF (_, paintOrder)) m
        | Just (SetGeometry rect) <- fromMessage m =
            Just $ PSF (Just rect, paintOrder)
        | otherwise = Nothing

updatePositionStore :: (Window, Rectangle) -> Rectangle -> X ()
updatePositionStore (w, rect) sr = modifyPosStore (\ps ->
                                            posStoreInsert ps w rect sr)

reorder :: (Eq a) => [(a, b)] -> [a] -> [(a, b)]
reorder wrs order =
    let ordered = concatMap (pickElem wrs) order
        rest = filter (\(w, _) -> w `notElem` order) wrs
    in ordered ++ rest
    where
        pickElem list e = case lookup e list of
                                Just result -> [(e, result)]
                                Nothing -> []
