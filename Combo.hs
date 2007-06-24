-----------------------------------------------------------------------------
-- |
-- Module      :  XMonadContrib.Combo
-- Copyright   :  (c) David Roundy <droundy@darcs.net>
-- License     :  BSD-style (see LICENSE)
-- 
-- Maintainer  :  David Roundy <droundy@darcs.net>
-- Stability   :  unstable
-- Portability :  unportable
--
-- A layout that combines multiple layouts.
--
-----------------------------------------------------------------------------

module XMonadContrib.Combo (
                            -- * Usage
                            -- $usage 
                            combo
                           ) where

import Control.Arrow ( first )
import Data.Maybe ( isJust )
import XMonad
import StackSet ( integrate, differentiate )

-- $usage
--
-- To use this layout write, in your Config.hs:
-- 
-- > import XMonadContrib.Combo 
-- > import XMonadContrib.SimpleStacking
-- 
-- and add something like
-- 
-- > simpleStacking $ combo (twoPane 0.03 0.5) [(full,1),(tabbed shrinkText,1)]
--
-- to your defaultLayouts.
--
-- The first argument to combo is a Layout that will divide the screen into
-- one or more subscreens.  The second argument is a list of layouts which
-- will be used to lay out the contents of each of those subscreents.
-- Paired with each of these layouts is an integer giving the number of
-- windows this section should hold.  This number is ignored for the last
-- layout, which will hold any excess windows.

combo :: Layout (Layout a, Int) -> [(Layout a, Int)] -> Layout a
combo super origls = Layout { doLayout = \r s -> arrange r (integrate s), modifyLayout = message }
    where arrange _ [] = return ([], Nothing)
          arrange r [w] = return ([(w,r)], Nothing)
          arrange rinput origws =
              do lrs <- fst `fmap`
                       runLayout super rinput (differentiate $ take (length origws) origls)
                 let lwrs [] _ = []
                     lwrs [((l,_),r)] ws = [((l,r),differentiate ws)]
                     lwrs (((l,n),r):xs) ws = ((l,r),differentiate $ take len1 ws) : lwrs xs (drop len1 ws)
                         where len1 = min n (length ws - length xs)
                 out <- mapM (uncurry $ uncurry runLayout) $ lwrs lrs origws
                 let origls' = zipWith foo (out++repeat ([],Nothing)) origls
                     foo (_, Nothing) x = x
                     foo (_, Just l') (_, n) = (l', n)
                 return (concat $ map fst out, Just $ combo super origls')
          message m = do mls <- broadcastPrivate m (map fst origls)
                         let mls' = (\x->zipWith first (map const x) origls) `fmap` mls
                         msuper <- broadcastPrivate m [super]
                         case msuper of
                           Just [super'] -> return $ Just $ combo super' $ maybe origls id mls'
                           _ -> return $ combo super `fmap` mls'

broadcastPrivate :: SomeMessage -> [Layout b] -> X (Maybe [Layout b])
broadcastPrivate a ol = do nml <- mapM f ol
                           if any isJust nml
                              then return $ Just $ zipWith ((flip maybe) id) ol nml
                              else return Nothing
    where f l = modifyLayout l a `catchX` return Nothing
