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

module XMonad.Layout.Selective (
    -- * Description
    -- $description
    -- * Usage
    -- $usage

    -- The Layout Modifier
    selective
    ) where

import XMonad.Core
import XMonad.StackSet
import XMonad.Layout (IncMasterN (..))
import XMonad.Layout.LayoutModifier
import Control.Applicative ((<$>))

-- $description
-- Selective is a layout modifier which limits the number of windows on screen.
-- The first @n@ windows ("the master pane", which may correspond to the
-- master pane of the underlying layout) plus several others are shown, such
-- that the focussed window is always visible. Windows are not moved until a
-- hidden window gains focus.

-- $usage
-- To use this module, add the following import to @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Layout.Selective
--
-- > myLayout = (selective 1 3 $ Tall 1 0.03 0.5) ||| Full ||| RandomOtherLayout...
-- > main = xmonad defaultConfig { layoutHook = myLayout }
--
-- The layout modifier accepts the IncMasterN message to change the number of
-- windows in the "master pane".
--
-- For detailed instructions on editing your key bindings, see
-- "XMonad.Doc.Extending#Editing_key_bindings".
--
-- See also 'XMonad.Layout.BoringWindows.boringAuto' for keybindings that skip
-- the hidden windows.

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

-- | Only display the first @m@ windows and @r@ others.
-- The @IncMasterN@ message will change @m@, as well as passing it onto the
-- underlying layout.
selective :: Int -> Int -> l a -> ModifiedLayout Selective l a
selective m r = ModifiedLayout . Selective $ Sel { nMaster=m, start=m, nRest=r }
