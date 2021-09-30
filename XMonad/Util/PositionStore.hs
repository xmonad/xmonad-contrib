----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Util.PositionStore
-- Description :  A utility module to store information about position and size of a window.
-- Copyright   :  (c) Jan Vornberger 2009
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  jan.vornberger@informatik.uni-oldenburg.de
-- Stability   :  unstable
-- Portability :  not portable
--
-- A utility module to store information about position and size of a window.
-- See "XMonad.Layout.PositionStoreFloat" for a layout that makes use of this.
--
-----------------------------------------------------------------------------

module XMonad.Util.PositionStore (
        getPosStore,
        modifyPosStore,

        posStoreInsert,
        posStoreMove,
        posStoreQuery,
        posStoreRemove,
        PositionStore,
    ) where

import XMonad
import qualified XMonad.Util.ExtensibleState as XS
import qualified Data.Map as M

-- Store window positions relative to the upper left screen edge
-- and windows sizes as well as positions as fractions of the screen size.
-- This way windows can be easily relocated and scaled when switching screens.

newtype PositionStore = PS (M.Map Window PosStoreRectangle)
                            deriving (Read,Show)
data PosStoreRectangle = PSRectangle Double Double Double Double
                            deriving (Read,Show)

instance ExtensionClass PositionStore where
  initialValue = PS M.empty
  extensionType = PersistentExtension

getPosStore :: X PositionStore
getPosStore = XS.get

modifyPosStore :: (PositionStore -> PositionStore) -> X ()
modifyPosStore = XS.modify

posStoreInsert :: PositionStore -> Window -> Rectangle -> Rectangle -> PositionStore
posStoreInsert (PS posStoreMap) w (Rectangle x y wh ht) (Rectangle srX srY srWh srHt) =
    let offsetX = x - srX
        offsetY = y - srY
    in PS $ M.insert w (PSRectangle (fromIntegral offsetX / fromIntegral srWh)
                                               (fromIntegral offsetY / fromIntegral srHt)
                                               (fromIntegral wh / fromIntegral srWh)
                                               (fromIntegral ht / fromIntegral srHt)) posStoreMap

posStoreRemove :: PositionStore -> Window -> PositionStore
posStoreRemove (PS posStoreMap) w = PS $ M.delete w posStoreMap

posStoreQuery :: PositionStore -> Window -> Rectangle -> Maybe Rectangle
posStoreQuery (PS posStoreMap) w (Rectangle srX srY srWh srHt) = do
    (PSRectangle x y wh ht) <- M.lookup w posStoreMap
    let realWh = fromIntegral srWh * wh
        realHt = fromIntegral srHt * ht
        realOffsetX = fromIntegral srWh * x
        realOffsetY = fromIntegral srHt * y
    return (Rectangle (srX + round realOffsetX) (srY + round realOffsetY)
                        (round realWh) (round realHt))

posStoreMove :: PositionStore -> Window -> Position -> Position -> Rectangle -> Rectangle -> PositionStore
posStoreMove posStore w x y oldSr newSr =
    case posStoreQuery posStore w oldSr of
        Nothing -> posStore     -- not in store, can't move -> do nothing
        Just (Rectangle _ _ wh ht) -> posStoreInsert posStore w (Rectangle x y wh ht) newSr
