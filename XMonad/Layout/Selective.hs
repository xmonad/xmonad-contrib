-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.Selective
-- Copyright   :  (c) 2009 Max Rabkin
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  Max Rabkin <max.rabkin@gmail.com>
-- Stability   :  unstable
-- Portability :  unportable
--
-- Provides a layout modifier that only shows the master pane and windows
-- around the focussed window.
--
-----------------------------------------------------------------------------

{-# LANGUAGE MultiParamTypeClasses,
             FlexibleInstances,
             NoMonomorphismRestriction,
             NamedFieldPuns #-}

module XMonad.Layout.Selective where

import XMonad.Core
import XMonad.StackSet
import XMonad.Layout (IncMasterN (..))
import XMonad.Layout.LayoutModifier
import Control.Applicative ((<$>))

-- invariant: 0 <= nMaster <= start; 1 <= nRest
data Selection = Sel { nMaster :: Int, start :: Int, nRest :: Int }
    deriving (Read, Show, Eq)

select :: Selection -> Stack a -> Stack a
select (Sel { nMaster, start, nRest }) stk
    | lups < nMaster
        = stk { down=take (nMaster - lups - 1) downs ++
                    (take nRest . drop (start - lups - 1) $ downs) }
    | otherwise
        = stk { up=reverse (take nMaster ups ++ drop start ups),
                down=take (nRest - (lups - start) - 1) downs }
    where
        downs = down stk
        ups = reverse $ up stk
        lups = length ups
    
updateStart :: Selection -> Stack a -> Int
updateStart (Sel { nMaster, start, nRest }) stk
    | lups < nMaster   -- the focussed window is in the master pane
        = start `min` (lups + ldown - nRest + 1) `max` nMaster
    | otherwise
        = start `min` lups 
                `max` (lups - nRest + 1) 
                `min` (lups + ldown - nRest + 1) 
                `max` nMaster
    where
        lups = length $ up stk
        ldown = length $ down stk

update :: Selection -> Stack a -> Selection
update sel stk = sel { start=updateStart sel stk }

updateAndSelect :: Selection -> Stack a -> Stack a
updateAndSelect sel stk = select (update sel stk) stk

data Selective a = Selective Selection
    deriving (Read, Show)

instance LayoutModifier Selective a where
    modifyLayout (Selective s) w r =
        runLayout (w { stack = updateAndSelect s <$> stack w }) r

    pureModifier (Selective sel) _ stk wins = (wins, Selective . update sel <$> stk)

    pureMess (Selective s) m = Selective . incmastern <$> fromMessage m
        where
            incmastern (IncMasterN n) =
                let nm = (nMaster s + n) `max` 0
                in if nMaster s == start s 
                    then s { nMaster = nm, start = nm }
                    else s { nMaster = nm }

selective :: Int -> Int -> l a -> ModifiedLayout Selective l a
selective m r = ModifiedLayout . Selective $ Sel { nMaster=m, start=m, nRest=r }
