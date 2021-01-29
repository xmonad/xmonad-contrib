{-# LANGUAGE RankNTypes #-}
module Utils where -- copied from the core library

import XMonad.StackSet hiding (filter)
import Graphics.X11.Xlib.Types (Rectangle(..))
import Data.List (sortBy)

-- Useful operation, the non-local workspaces
hidden_spaces :: StackSet i l a sid sd -> [Workspace i l a]
hidden_spaces x = map workspace (visible x) ++ hidden x


-- normalise workspace list
normal :: Ord i => StackSet i l a s sd -> StackSet i l a s sd
normal s = s { hidden = sortBy g (hidden s), visible = sortBy f (visible s) }
    where
        f a b = tag (workspace a) `compare` tag (workspace b)
        g a b = tag a `compare` tag b


noOverlaps :: [Rectangle] -> Bool
noOverlaps []  = True
noOverlaps [_] = True
noOverlaps xs  = and [ verts a `notOverlap` verts b
                     | a <- xs
                     , b <- filter (a /=) xs
                     ]
    where
      verts (Rectangle a b w h) = (a,b,a + fromIntegral w - 1, b + fromIntegral h - 1)

      notOverlap (left1,bottom1,right1,top1)
                 (left2,bottom2,right2,top2)
        =  (top1 < bottom2 || top2 < bottom1)
        || (right1 < left2 || right2 < left1)


applyN :: (Integral n) => Maybe n -> (a -> a) -> a -> a
applyN Nothing  _ v = v
applyN (Just 0) _ v = v
applyN (Just n) f v = applyN (Just $ n - 1) f (f v)

tags :: StackSet i l a sid sd -> [i]
tags x = map tag $ workspaces x


-- | noOverflows op a b is True if @a `op` fromIntegral b@ overflows (or
-- otherwise gives the same answer when done using Integer
noOverflows :: (Integral b, Integral c) =>
  (forall a. Integral a => a -> a -> a) -> b -> c -> Bool
noOverflows op a b = toInteger (a `op` fromIntegral b) == toInteger a `op` toInteger b
