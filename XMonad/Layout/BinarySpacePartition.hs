{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, DeriveDataTypeable #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.BinarySpacePartition
-- Copyright   :  (c) 2013 Ben Weitzman <benweitzman@gmail.com>
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  Ben Weitzman <benweitzman@gmail.com>
-- Stability   :  unstable
-- Portability :  unportable
--
-- Layout where new windows will split the focused window in half, based off of BSPWM
--
-----------------------------------------------------------------------------
 
module XMonad.Layout.BinarySpacePartition (
                                          -- * Usage
                                          -- $usage
                                            emptyBSP
                                          , Rotate(..)
                                          , Swap(..)
                                          , ResizeDirectional(..)
                                          , Bound(..)
                                          ) where

import XMonad
import qualified XMonad.StackSet as W
import XMonad.Util.Stack hiding (Zipper)
import qualified Data.Map as M
import Data.List ((\\))
import Control.Monad

-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
-- 
-- > import XMonad.Layout.BinarySpacePartition
--    
-- Then add the layout, using the default BSP (BinarySpacePartition)
--
-- > myLayout = emptyBSP ||| etc ..
--     
-- It will be helpful to add the following key bindings
--      
-- > , ((modm .|. altMask,               xK_l     ), sendMessage $ ExpandTowards East)
-- > , ((modm .|. altMask,               xK_h     ), sendMessage $ ExpandTowards West)
-- > , ((modm .|. altMask,               xK_j     ), sendMessage $ ExpandTowards South)
-- > , ((modm .|. altMask,               xK_k     ), sendMessage $ ExpandTowards North)
-- > , ((modm .|. altMask .|. ctrlMask , xK_l     ), sendMessage $ ShrinkFrom East)
-- > , ((modm .|. altMask .|. ctrlMask , xK_h     ), sendMessage $ ShrinkFrom West)
-- > , ((modm .|. altMask .|. ctrlMask , xK_j     ), sendMessage $ ShrinkFrom South)
-- > , ((modm .|. altMask .|. ctrlMask , xK_k     ), sendMessage $ ShrinkFrom North)
-- > , ((modm,                           xK_r     ), sendMessage Rotate)
-- > , ((modm,                           xK_s     ), sendMessage Swap)
-- 

-- |Message for rotating a split in the BSP. Keep in mind that this does not change the order
-- of the windows, it will just turn a horizontal split into a verticial one and vice versa
data Rotate = Rotate deriving Typeable
instance Message Rotate

-- |Message for resizing one of the cells in the BSP
data ResizeDirectional = ExpandTowards Bound | ShrinkFrom Bound deriving Typeable
instance Message ResizeDirectional

-- |Message for swapping the left child of a split with the right child of split.
-- Keep in mind that it does not change the order of windows and will seem to have bizarre effects
-- if you are not expecting them.
data Swap = Swap deriving Typeable
instance Message Swap

data Bound = East | West | North | South deriving Typeable

data Direction = Horizontal | Vertical deriving (Show, Read, Eq)

oppositeDirection :: Direction -> Direction
oppositeDirection Vertical = Horizontal
oppositeDirection Horizontal = Vertical

split :: Direction -> Rational -> Rectangle -> (Rectangle, Rectangle)
split Horizontal r (Rectangle sx sy sw sh) = (r1, r2) where 
    r1 = Rectangle sx sy sw sh'
    r2 = Rectangle sx (sy + fromIntegral sh') sw (sh - sh')
    sh' = floor $ fromIntegral sh * r
split Vertical r (Rectangle sx sy sw sh) = (r1, r2) where
    r1 = Rectangle sx sy sw' sh
    r2 = Rectangle (sx + fromIntegral sw') sy (sw - sw') sh
    sw' = floor $ fromIntegral sw * r

data Split = Split { direction :: Direction
                   , ratio :: Rational
                   } deriving (Show, Read, Eq)
                              
oppositeSplit :: Split -> Split
oppositeSplit (Split d r) = Split (oppositeDirection d) r

increaseRatio :: Split -> Rational -> Split
increaseRatio (Split d r) delta = Split d (min 0.9 (max 0.1 (r + delta))) 

data Tree a = Leaf | Node { value :: a
                          , left :: Tree a
                          , right :: Tree a 
                          } deriving (Show, Read, Eq)

numLeaves :: Tree a -> Int
numLeaves Leaf = 1
numLeaves (Node _ l r) = numLeaves l + numLeaves r

data Crumb a = LeftCrumb a (Tree a) | RightCrumb a (Tree a) deriving (Show, Read, Eq)

swapCrumb :: Crumb a -> Crumb a
swapCrumb (LeftCrumb s t) = RightCrumb s t
swapCrumb (RightCrumb s t) = LeftCrumb s t

parentVal :: Crumb a -> a
parentVal (LeftCrumb s _) = s
parentVal (RightCrumb s _) = s

modifyParentVal :: (a -> a) -> Crumb a -> Crumb a
modifyParentVal f (LeftCrumb s t) = LeftCrumb (f s) t
modifyParentVal f (RightCrumb s t) = RightCrumb (f s) t

type Zipper a = (Tree a, [Crumb a])

toZipper :: Tree a -> Zipper a
toZipper t = (t, [])

goLeft :: Zipper a -> Maybe (Zipper a)
goLeft (Leaf, _) = Nothing
goLeft (Node x l r, bs) = Just (l, LeftCrumb x r:bs)

goRight :: Zipper a -> Maybe (Zipper a)
goRight (Leaf, _) = Nothing
goRight (Node x l r, bs) = Just (r, RightCrumb x l:bs)

goUp :: Zipper a -> Maybe (Zipper a)
goUp (_, []) = Nothing
goUp (t, LeftCrumb x r:cs) = Just (Node x t r, cs)
goUp (t, RightCrumb x l:cs) = Just (Node x l t, cs)

goSibling :: Zipper a -> Maybe (Zipper a)
goSibling (_, []) = Nothing
goSibling z@(_, LeftCrumb _ _:_) = Just z >>= goUp >>= goRight
goSibling z@(_, RightCrumb _ _:_) = Just z >>= goUp >>= goLeft

goToNthLeaf :: Int -> Zipper a -> Maybe (Zipper a)
goToNthLeaf _ z@(Leaf, _) = Just z
goToNthLeaf n z@(t, _) = 
  if numLeaves (left t) > n
  then do z' <- goLeft z
          goToNthLeaf n z'
  else do z' <- goRight z
          goToNthLeaf (n - (numLeaves . left $ t)) z'
          
splitCurrentLeaf :: Zipper Split -> Maybe (Zipper Split)                  
splitCurrentLeaf (Leaf, []) = Just (Node (Split Vertical 0.5) Leaf Leaf, [])
splitCurrentLeaf (Leaf, crumb:cs) = Just (Node (Split (oppositeDirection . direction . parentVal $ crumb) 0.5) Leaf Leaf, crumb:cs)
splitCurrentLeaf _ = Nothing

removeCurrentLeaf :: Zipper a -> Maybe (Zipper a)
removeCurrentLeaf (Leaf, []) = Nothing
removeCurrentLeaf (Leaf, LeftCrumb _ r:cs) = Just (r, cs)
removeCurrentLeaf (Leaf, RightCrumb _ l:cs) = Just (l, cs)
removeCurrentLeaf _ = Nothing

rotateCurrentLeaf :: Zipper Split -> Maybe (Zipper Split)
rotateCurrentLeaf (Leaf, []) = Just (Leaf, [])
rotateCurrentLeaf (Leaf, c:cs) = Just (Leaf, modifyParentVal oppositeSplit c:cs)
rotateCurrentLeaf _ = Nothing

swapCurrentLeaf :: Zipper a -> Maybe (Zipper a)
swapCurrentLeaf (Leaf, []) = Just (Leaf, [])
swapCurrentLeaf (Leaf, c:cs) = Just (Leaf, swapCrumb c:cs) 
swapCurrentLeaf _ = Nothing

expandTreeTowards :: Bound -> Zipper Split -> Maybe (Zipper Split)
expandTreeTowards _ z@(_, []) = Just z
expandTreeTowards East (t, LeftCrumb s r:cs) 
  | direction s == Vertical = Just (t, LeftCrumb (increaseRatio s 0.1) r:cs)
expandTreeTowards West (t, RightCrumb s l:cs)                              
  | direction s == Vertical = Just (t, RightCrumb (increaseRatio s (-0.1)) l:cs)
expandTreeTowards South (t, LeftCrumb s r:cs)                              
  | direction s == Horizontal = Just (t, LeftCrumb (increaseRatio s 0.1) r:cs)
expandTreeTowards North (t, RightCrumb s l:cs)                                 
  | direction s == Horizontal = Just (t, RightCrumb (increaseRatio s (-0.1)) l:cs)
expandTreeTowards dir z = goUp z >>= expandTreeTowards dir
                              
shrinkTreeFrom :: Bound -> Zipper Split -> Maybe (Zipper Split)                          
shrinkTreeFrom _ z@(_, []) = Just z
shrinkTreeFrom East z@(_, LeftCrumb s _:_) 
  | direction s == Vertical = Just z >>= goSibling >>= expandTreeTowards West
shrinkTreeFrom West z@(_, RightCrumb s _:_)
  | direction s == Vertical = Just z >>= goSibling >>= expandTreeTowards East
shrinkTreeFrom South z@(_, LeftCrumb s _:_)                              
  | direction s == Horizontal = Just z >>= goSibling >>= expandTreeTowards North
shrinkTreeFrom North z@(_, RightCrumb s _:_)                                 
  | direction s == Horizontal = Just z >>= goSibling >>= expandTreeTowards South
shrinkTreeFrom dir z = goUp z >>= shrinkTreeFrom dir
                       
top :: Zipper a -> Zipper a
top z = case goUp z of
          Nothing -> z
          Just z' -> top z'

toTree :: Zipper a -> Tree a
toTree = fst . top

index :: W.Stack a -> Int
index s = case toIndex (Just s) of 
            (_, Nothing) -> 0
            (_, Just int) -> int
                       
data BinarySpacePartition a = BinarySpacePartition { getTree :: Maybe (Tree Split) } deriving (Show, Read)

-- | an empty BinarySpacePartition to use as a default for adding windows to. 
emptyBSP :: BinarySpacePartition a
emptyBSP = BinarySpacePartition Nothing

makeBSP :: Tree Split -> BinarySpacePartition a
makeBSP = BinarySpacePartition . Just

makeZipper :: BinarySpacePartition a -> Maybe (Zipper Split)
makeZipper (BinarySpacePartition Nothing) = Nothing
makeZipper (BinarySpacePartition (Just t)) = Just . toZipper $ t

size :: BinarySpacePartition a -> Int
size = maybe 0 numLeaves . getTree

zipperToBinarySpacePartition :: Maybe (Zipper Split) -> BinarySpacePartition b
zipperToBinarySpacePartition Nothing = BinarySpacePartition Nothing
zipperToBinarySpacePartition (Just z) = BinarySpacePartition . Just . toTree . top $ z

rectangles :: BinarySpacePartition a -> Rectangle -> [Rectangle]
rectangles (BinarySpacePartition Nothing) _ = []
rectangles (BinarySpacePartition (Just Leaf)) rootRect = [rootRect]
rectangles (BinarySpacePartition (Just node)) rootRect = 
    rectangles (makeBSP . left $ node) leftBox ++ 
    rectangles (makeBSP . right $ node) rightBox 
    where (leftBox, rightBox) = split (direction info) (ratio info) rootRect
          info = value node

doToNth :: (Zipper Split -> Maybe (Zipper Split)) -> BinarySpacePartition a -> Int -> BinarySpacePartition a
doToNth f b n = zipperToBinarySpacePartition $ makeZipper b >>= goToNthLeaf n >>= f

splitNth :: BinarySpacePartition a -> Int -> BinarySpacePartition a
splitNth (BinarySpacePartition Nothing) _ = makeBSP Leaf
splitNth b n = doToNth splitCurrentLeaf b n 

removeNth :: BinarySpacePartition a -> Int -> BinarySpacePartition a                    
removeNth (BinarySpacePartition Nothing) _ = emptyBSP
removeNth (BinarySpacePartition (Just Leaf)) _ = emptyBSP
removeNth b n = doToNth removeCurrentLeaf b n 
                    
rotateNth :: BinarySpacePartition a -> Int -> BinarySpacePartition a                    
rotateNth (BinarySpacePartition Nothing) _ = emptyBSP
rotateNth b@(BinarySpacePartition (Just Leaf)) _ = b
rotateNth b n = doToNth rotateCurrentLeaf b n 
                                        
swapNth :: BinarySpacePartition a -> Int -> BinarySpacePartition a
swapNth (BinarySpacePartition Nothing) _ = emptyBSP
swapNth b@(BinarySpacePartition (Just Leaf)) _ = b
swapNth b n = doToNth swapCurrentLeaf b n 
                    
growNthTowards :: Bound -> BinarySpacePartition a -> Int -> BinarySpacePartition a
growNthTowards _ (BinarySpacePartition Nothing) _ = emptyBSP
growNthTowards _ b@(BinarySpacePartition (Just Leaf)) _ = b
growNthTowards dir b n = doToNth (expandTreeTowards dir) b n 
                    
shrinkNthFrom :: Bound -> BinarySpacePartition a -> Int -> BinarySpacePartition a                    
shrinkNthFrom _ (BinarySpacePartition Nothing) _ = emptyBSP
shrinkNthFrom _ b@(BinarySpacePartition (Just Leaf)) _ = b
shrinkNthFrom dir b n = doToNth (shrinkTreeFrom dir) b n 

instance LayoutClass BinarySpacePartition a where
  doLayout b r s = return (zip ws rs, layout b) where
    ws = W.integrate s
    layout bsp 
      | l == count = Just bsp
      | l > count = layout $ splitNth bsp n
      | otherwise = layout $ removeNth bsp n 
      where count = size bsp
                        
    l = length ws
    n = index s    
    rs = case layout b of
      Nothing -> rectangles b r
      Just bsp' -> rectangles bsp' r
  handleMessage b m =
    do ms <- (W.stack . W.workspace . W.current) `fmap` gets windowset
       fs <- (M.keys . W.floating) `fmap` gets windowset
       return $ ms >>= unfloat fs >>= handleMesg
    where handleMesg s = msum [fmap (`rotate` s) (fromMessage m)
                              ,fmap (`resize` s) (fromMessage m)
                              ,fmap (`swap` s) (fromMessage m)
                              ]
          unfloat fs s = if W.focus s `elem` fs
                         then Nothing
                         else Just (s { W.up = W.up s \\ fs
                                      , W.down = W.down s \\ fs })
          rotate Rotate s = rotateNth b $ index s
          swap Swap s = swapNth b $ index s
          resize (ExpandTowards dir) s = growNthTowards dir b $ index s
          resize (ShrinkFrom dir) s = shrinkNthFrom dir b $ index s
  description _  = "BSP"

