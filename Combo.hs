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
-- > simpleStacking $ combo [(full,1),(tabbed shrinkText,1)] (twoPane 0.03 0.5)
--
-- to your defaultLayouts.

combo :: [(Layout a, Int)] -> Layout a -> Layout a
combo origls super = Layout { doLayout = \r s -> arrange r (integrate s), modifyLayout = message }
    where arrange _ [] = return ([], Nothing)
          arrange r [w] = return ([(w,r)], Nothing)
          arrange rinput origws =
              do rs <- (map snd . fst) `fmap`
                       runLayout super rinput (differentiate $ take (length origls) origws)
                 let wss [] _ = []
                     wss [_] ws = [ws]
                     wss (n:ns) ws = take len1 ws : wss ns (drop len1 ws)
                         where len1 = min n (length ws - length ns)
                 out <- sequence $ zipWith3 runLayout (map fst origls) rs
                                                      (map differentiate $
                                                       wss (take (length rs) $ map snd origls) origws)
                 let origls' = zipWith foo (out++repeat ([],Nothing)) origls
                     foo (_, Nothing) x = x
                     foo (_, Just l') (_, n) = (l', n)
                 return (concat $ map fst out, Just $ combo origls' super)
          message m = do mls <- broadcastPrivate m (super:map fst origls)
                         return $ (\(super':ls') -> combo (zip ls' $ map snd origls) super') `fmap` mls

broadcastPrivate :: SomeMessage -> [Layout b] -> X (Maybe [Layout b])
broadcastPrivate a ol = do nml <- mapM f ol
                           if any isJust nml
                              then return $ Just $ zipWith ((flip maybe) id) ol nml
                              else return Nothing
    where f l = modifyLayout l a `catchX` return Nothing
