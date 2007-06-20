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

import XMonad
import StackSet ( integrate, differentiate )
import Operations ( UnDoLayout(UnDoLayout) )

-- $usage
--
-- To use this layout write, in your Config.hs:
-- 
-- > import XMonadContrib.Combo 
-- 
-- and add something like
-- 
-- > combo [(full,1),(tabbed shrinkText,1)] (twoPane 0.03 0.5)
--
-- to your defaultLayouts.

combo :: [(Layout a, Int)] -> Layout a -> Layout a
combo origls super = Layout { doLayout = \r s -> arrange r (integrate s), modifyLayout = message }
    where arrange _ [] = return []
          arrange r [w] = return [(w,r)]
          arrange rinput origws =
              do rs <- map snd `fmap` runLayout super rinput (differentiate $ take (length origls) origws)
                 let wss [] _ = []
                     wss [_] ws = [ws]
                     wss (n:ns) ws = take len1 ws : wss ns (drop len1 ws)
                         where len1 = min n (length ws - length ns)
                 out <- sequence $ zipWith3 runLayout (map fst origls) rs
                                                      (map differentiate $
                                                       wss (take (length rs) $ map snd origls) origws)
                 return $ concat out
          message m = case fromMessage m of
                        Just UnDoLayout -> fmap (\(super':ls') -> Just $ combo (zip ls' $ map snd origls) super')
                                                (broadcastPrivate UnDoLayout (super:map fst origls))
                        _               -> fmap (maybe Nothing (Just . combo origls)) (modifyLayout super m)

broadcastPrivate :: Message a => a -> [Layout b] -> X [Layout b]
broadcastPrivate a ol = mapM f ol
    where f l = do ml' <- modifyLayout l (SomeMessage a) `catchX` return (Just l)
                   return $ maybe l id ml'
