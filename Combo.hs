-- A layout that combines multiple layouts.

-- To use this layout, 'import XMonadContrib.Combo' and add something like
-- 'combo [(full,1),(tabbed,1)] (twoPane 0.03 0.5)' to your defaultLayouts.

module XMonadContrib.Combo where

import XMonad
import StackSet ( integrate, differentiate )

combo :: [(Layout, Int)] -> Layout -> Layout
combo origls super = Layout { doLayout = \r s -> arrange r (integrate s), modifyLayout = message }
    where arrange _ [] = return []
          arrange r [w] = return [(w,r)]
          arrange rinput origws =
              do rs <- map snd `fmap` doLayout super rinput (differentiate $ take (length origls) origws)
                 let wss [] _ = []
                     wss [_] ws = [ws]
                     wss (n:ns) ws = take len1 ws : wss ns (drop len1 ws)
                         where len1 = min n (length ws - length ns)
                 out <- sequence $ zipWith3 doLayout (map fst origls) rs
                                                     (map differentiate $
                                                      wss (take (length rs) $ map snd origls) origws)
                 return $ concat out
          message m = do msuper' <- modifyLayout super m
                         case msuper' of
                           Nothing -> return Nothing
                           Just super' -> return $ Just $ combo origls super'
