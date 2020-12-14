{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, DeriveDataTypeable #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.BinarySpacePartition
-- Copyright   :  (c) 2013 Ben Weitzman    <benweitzman@gmail.com>
--                    2015 Anton Pirogov   <anton.pirogov@gmail.com>
--                    2019 Mateusz Karbowy <obszczymucha@gmail.com
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
  , BinarySpacePartition
  , Rotate(..)
  , Swap(..)
  , ResizeDirectional(..)
  , TreeRotate(..)
  , TreeBalance(..)
  , FocusParent(..)
  , SelectMoveNode(..)
  , Direction2D(..)
  , SplitShiftDirectional(..)
  ) where

import XMonad
import qualified XMonad.StackSet as W
import XMonad.Util.Stack hiding (Zipper)
import XMonad.Util.Types

-- for mouse resizing
import XMonad.Layout.WindowArranger (WindowArrangerMsg(SetGeometry))
-- for "focus parent" node border
import XMonad.Util.XUtils

import qualified Data.Map as M
import qualified Data.Set as S
import Data.List ((\\), elemIndex, foldl')
import Data.Maybe (fromMaybe, isNothing, isJust, mapMaybe, catMaybes)
import Control.Monad
import Data.Ratio ((%))

-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Layout.BinarySpacePartition
--
-- Then add the layout, using the default BSP (BinarySpacePartition)
--
-- > myLayout = emptyBSP ||| etc ..
--
-- It may be a good idea to use "XMonad.Actions.Navigation2D" to move between the windows.
--
-- This layout responds to SetGeometry and is compatible with e.g. "XMonad.Actions.MouseResize"
-- or "XMonad.Layout.BorderResize". You should probably try both to decide which is better for you,
-- if you want to be able to resize the splits with the mouse.
--
-- If you don't want to use the mouse, add the following key bindings to resize the splits with the keyboard:
--
-- > , ((modm .|. altMask,                 xK_l     ), sendMessage $ ExpandTowards R)
-- > , ((modm .|. altMask,                 xK_h     ), sendMessage $ ExpandTowards L)
-- > , ((modm .|. altMask,                 xK_j     ), sendMessage $ ExpandTowards D)
-- > , ((modm .|. altMask,                 xK_k     ), sendMessage $ ExpandTowards U)
-- > , ((modm .|. altMask .|. ctrlMask ,   xK_l     ), sendMessage $ ShrinkFrom R)
-- > , ((modm .|. altMask .|. ctrlMask ,   xK_h     ), sendMessage $ ShrinkFrom L)
-- > , ((modm .|. altMask .|. ctrlMask ,   xK_j     ), sendMessage $ ShrinkFrom D)
-- > , ((modm .|. altMask .|. ctrlMask ,   xK_k     ), sendMessage $ ShrinkFrom U)
-- > , ((modm,                             xK_r     ), sendMessage Rotate)
-- > , ((modm,                             xK_s     ), sendMessage Swap)
-- > , ((modm,                             xK_n     ), sendMessage FocusParent)
-- > , ((modm .|. ctrlMask,                xK_n     ), sendMessage SelectNode)
-- > , ((modm .|. shiftMask,               xK_n     ), sendMessage MoveNode)
-- > , ((modm .|. shiftMask .|. ctrlMask , xK_j     ), sendMessage $ SplitShift Prev)
-- > , ((modm .|. shiftMask .|. ctrlMask , xK_k     ), sendMessage $ SplitShift Next)
--
-- Here's an alternative key mapping, this time using additionalKeysP,
-- arrow keys, and slightly different behavior when resizing windows
--
-- > , ("M-M1-<Left>",    sendMessage $ ExpandTowards L)
-- > , ("M-M1-<Right>",   sendMessage $ ShrinkFrom L)
-- > , ("M-M1-<Up>",      sendMessage $ ExpandTowards U)
-- > , ("M-M1-<Down>",    sendMessage $ ShrinkFrom U)
-- > , ("M-M1-C-<Left>",  sendMessage $ ShrinkFrom R)
-- > , ("M-M1-C-<Right>", sendMessage $ ExpandTowards R)
-- > , ("M-M1-C-<Up>",    sendMessage $ ShrinkFrom D)
-- > , ("M-M1-C-<Down>",  sendMessage $ ExpandTowards D)
-- > , ("M-s",            sendMessage $ BSP.Swap)
-- > , ("M-M1-s",         sendMessage $ Rotate)
-- > , ("M-S-C-j",        sendMessage $ SplitShift Prev)
-- > , ("M-S-C-k",        sendMessage $ SplitShift Next)
--
-- If you have many windows open and the layout begins to look too hard to manage, you can 'Balance'
-- the layout, so that the current splittings are discarded and windows are tiled freshly in a way that
-- the split depth is minimized. You can combine this with 'Equalize', which does not change your tree,
-- but tunes the split ratios in a way that each window gets the same amount of space:
--
-- > , ((myModMask,               xK_a),     sendMessage Balance)
-- > , ((myModMask .|. shiftMask, xK_a),     sendMessage Equalize)
--

-- |Message for rotating the binary tree around the parent node of the window to the left or right
data TreeRotate = RotateL | RotateR deriving Typeable
instance Message TreeRotate

-- |Message to balance the tree in some way (Balance retiles the windows, Equalize changes ratios)
data TreeBalance = Balance | Equalize deriving Typeable
instance Message TreeBalance

-- |Message for resizing one of the cells in the BSP
data ResizeDirectional = ExpandTowards Direction2D | ShrinkFrom Direction2D | MoveSplit Direction2D deriving Typeable
instance Message ResizeDirectional

-- |Message for rotating a split (horizontal/vertical) in the BSP
data Rotate = Rotate deriving Typeable
instance Message Rotate

-- |Message for swapping the left child of a split with the right child of split
data Swap = Swap deriving Typeable
instance Message Swap

-- |Message to cyclically select the parent node instead of the leaf
data FocusParent = FocusParent deriving Typeable
instance Message FocusParent

-- |Message to move nodes inside the tree
data SelectMoveNode = SelectNode | MoveNode deriving Typeable
instance Message SelectMoveNode

data Axis = Horizontal | Vertical deriving (Show, Read, Eq)

-- |Message for shifting window by splitting its neighbour
data SplitShiftDirectional = SplitShift Direction1D deriving Typeable
instance Message SplitShiftDirectional

oppositeDirection :: Direction2D -> Direction2D
oppositeDirection U = D
oppositeDirection D = U
oppositeDirection L = R
oppositeDirection R = L

oppositeAxis :: Axis -> Axis
oppositeAxis Vertical = Horizontal
oppositeAxis Horizontal = Vertical

toAxis :: Direction2D -> Axis
toAxis U = Horizontal
toAxis D = Horizontal
toAxis L = Vertical
toAxis R = Vertical

split :: Axis -> Rational -> Rectangle -> (Rectangle, Rectangle)
split Horizontal r (Rectangle sx sy sw sh) = (r1, r2) where
    r1 = Rectangle sx sy sw sh'
    r2 = Rectangle sx (sy + fromIntegral sh') sw (sh - sh')
    sh' = floor $ fromIntegral sh * r
split Vertical r (Rectangle sx sy sw sh) = (r1, r2) where
    r1 = Rectangle sx sy sw' sh
    r2 = Rectangle (sx + fromIntegral sw') sy (sw - sw') sh
    sw' = floor $ fromIntegral sw * r

data Split = Split { axis :: Axis
                   , ratio :: Rational
                   } deriving (Show, Read, Eq)

oppositeSplit :: Split -> Split
oppositeSplit (Split d r) = Split (oppositeAxis d) r

increaseRatio :: Split -> Rational -> Split
increaseRatio (Split d r) delta = Split d (min 0.9 (max 0.1 (r + delta)))

resizeDiff :: Rational
resizeDiff = 0.05


data Tree a = Leaf Int | Node { value :: a
                          , left :: Tree a
                          , right :: Tree a
                          } deriving (Show, Read, Eq)

numLeaves :: Tree a -> Int
numLeaves (Leaf _) = 1
numLeaves (Node _ l r) = numLeaves l + numLeaves r

-- right or left rotation of a (sub)tree, no effect if rotation not possible
rotTree :: Direction2D -> Tree a -> Tree a
rotTree _ (Leaf n) = Leaf n
rotTree R n@(Node _ (Leaf _) _) = n
rotTree L n@(Node _ _ (Leaf _)) = n
rotTree R (Node sp (Node sp2 l2 r2) r) = Node sp2 l2 (Node sp r2 r)
rotTree L (Node sp l (Node sp2 l2 r2)) = Node sp2 (Node sp l l2) r2
rotTree _ t = t


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
goLeft (Leaf _, _) = Nothing
goLeft (Node x l r, bs) = Just (l, LeftCrumb x r:bs)

goRight :: Zipper a -> Maybe (Zipper a)
goRight (Leaf _, _) = Nothing
goRight (Node x l r, bs) = Just (r, RightCrumb x l:bs)

goUp :: Zipper a -> Maybe (Zipper a)
goUp (_, []) = Nothing
goUp (t, LeftCrumb x r:cs) = Just (Node x t r, cs)
goUp (t, RightCrumb x l:cs) = Just (Node x l t, cs)

goSibling :: Zipper a -> Maybe (Zipper a)
goSibling (_, []) = Nothing
goSibling z@(_, LeftCrumb _ _:_) = Just z >>= goUp >>= goRight
goSibling z@(_, RightCrumb _ _:_) = Just z >>= goUp >>= goLeft

top :: Zipper a -> Zipper a
top z = case goUp z of
          Nothing -> z
          Just z' -> top z'

toTree :: Zipper a -> Tree a
toTree = fst . top

goToNthLeaf :: Int -> Zipper a -> Maybe (Zipper a)
goToNthLeaf _ z@(Leaf _, _) = Just z
goToNthLeaf n z@(t, _) =
  if numLeaves (left t) > n
  then do z' <- goLeft z
          goToNthLeaf n z'
  else do z' <- goRight z
          goToNthLeaf (n - (numLeaves . left $ t)) z'

toggleSplits :: Tree Split -> Tree Split
toggleSplits (Leaf l) = Leaf l
toggleSplits (Node s l r) = Node (oppositeSplit s) (toggleSplits l) (toggleSplits r)

splitCurrent :: Zipper Split -> Maybe (Zipper Split)
splitCurrent (Leaf _, []) = Just (Node (Split Vertical 0.5) (Leaf 0) (Leaf 0), [])
splitCurrent (Leaf _, crumb:cs) = Just (Node (Split (oppositeAxis . axis . parentVal $ crumb) 0.5) (Leaf 0) (Leaf 0), crumb:cs)
splitCurrent (n, []) = Just (Node (Split Vertical 0.5) (Leaf 0) (toggleSplits n), [])
splitCurrent (n, crumb:cs) = Just (Node (Split (oppositeAxis . axis . parentVal $ crumb) 0.5) (Leaf 0) (toggleSplits n), crumb:cs)

removeCurrent :: Zipper a -> Maybe (Zipper a)
removeCurrent (Leaf _, LeftCrumb _ r:cs) = Just (r, cs)
removeCurrent (Leaf _, RightCrumb _ l:cs) = Just (l, cs)
removeCurrent (Leaf _, []) = Nothing
removeCurrent (Node _ (Leaf _) r@(Node _ _ _), cs) = Just (r, cs)
removeCurrent (Node _ l@(Node _ _ _) (Leaf _), cs) = Just (l, cs)
removeCurrent (Node _ (Leaf _) (Leaf _), cs) = Just (Leaf 0, cs)
removeCurrent z@(Node _ _ _, _) = goLeft z >>= removeCurrent

rotateCurrent :: Zipper Split -> Maybe (Zipper Split)
rotateCurrent l@(_, []) = Just l
rotateCurrent (n, c:cs) = Just (n, modifyParentVal oppositeSplit c:cs)

swapCurrent :: Zipper a -> Maybe (Zipper a)
swapCurrent l@(_, []) = Just l
swapCurrent (n, c:cs) = Just (n, swapCrumb c:cs)

insertLeftLeaf :: Tree Split -> Zipper Split -> Maybe (Zipper Split)
insertLeftLeaf (Leaf n) ((Node x l r), crumb:cs) = Just (Node (Split (oppositeAxis . axis . parentVal $ crumb) 0.5) (Leaf n) (Node x l r), crumb:cs)
insertLeftLeaf (Leaf n) (Leaf x, crumb:cs) = Just (Node (Split (oppositeAxis . axis . parentVal $ crumb) 0.5) (Leaf n) (Leaf x), crumb:cs)
insertLeftLeaf (Node _ _ _) z = Just z

insertRightLeaf :: Tree Split -> Zipper Split -> Maybe (Zipper Split)
insertRightLeaf (Leaf n) ((Node x l r), crumb:cs) = Just (Node (Split (oppositeAxis . axis . parentVal $ crumb) 0.5) (Node x l r) (Leaf n), crumb:cs)
insertRightLeaf (Leaf n) (Leaf x, crumb:cs) = Just (Node (Split (oppositeAxis . axis . parentVal $ crumb) 0.5) (Leaf x) (Leaf n), crumb:cs)
insertRightLeaf (Node _ _ _) z = Just z

findRightLeaf :: Zipper Split -> Maybe (Zipper Split)
findRightLeaf n@(Node _ _ _, _) = goRight n >>= findRightLeaf
findRightLeaf l@(Leaf _, _) = Just l

findLeftLeaf :: Zipper Split -> Maybe (Zipper Split)
findLeftLeaf n@(Node _ _ _, _) = goLeft n
findLeftLeaf l@(Leaf _, _) = Just l

findTheClosestLeftmostLeaf :: Zipper Split -> Maybe (Zipper Split)
findTheClosestLeftmostLeaf s@(_, (RightCrumb _ _):_) = goUp s >>= goLeft >>= findRightLeaf
findTheClosestLeftmostLeaf s@(_, (LeftCrumb _ _):_) = goUp s >>= findTheClosestLeftmostLeaf

findTheClosestRightmostLeaf :: Zipper Split -> Maybe (Zipper Split)
findTheClosestRightmostLeaf s@(_, (RightCrumb _ _):_) = goUp s >>= findTheClosestRightmostLeaf
findTheClosestRightmostLeaf s@(_, (LeftCrumb _ _):_) = goUp s >>= goRight >>= findLeftLeaf

splitShiftLeftCurrent :: Zipper Split -> Maybe (Zipper Split)
splitShiftLeftCurrent l@(_, []) = Just l
splitShiftLeftCurrent l@(_, (RightCrumb _ _):_) = Just l -- Do nothing. We can swap windows instead.
splitShiftLeftCurrent l@(n, c:cs) = removeCurrent l >>= findTheClosestLeftmostLeaf >>= insertRightLeaf n

splitShiftRightCurrent :: Zipper Split -> Maybe (Zipper Split)
splitShiftRightCurrent l@(_, []) = Just l
splitShiftRightCurrent l@(_, (LeftCrumb _ _):_) = Just l -- Do nothing. We can swap windows instead.
splitShiftRightCurrent l@(n, c:cs) = removeCurrent l >>= findTheClosestRightmostLeaf >>= insertLeftLeaf n

isAllTheWay :: Direction2D -> Zipper Split -> Bool
isAllTheWay _ (_, []) = True
isAllTheWay R (_, LeftCrumb s _:_)
  | axis s == Vertical = False
isAllTheWay L (_, RightCrumb s _:_)
  | axis s == Vertical = False
isAllTheWay D (_, LeftCrumb s _:_)
  | axis s == Horizontal = False
isAllTheWay U (_, RightCrumb s _:_)
  | axis s == Horizontal = False
isAllTheWay dir z = fromMaybe False $ goUp z >>= Just . isAllTheWay dir

expandTreeTowards :: Direction2D -> Zipper Split -> Maybe (Zipper Split)
expandTreeTowards _ z@(_, []) = Just z
expandTreeTowards dir z
  | isAllTheWay dir z = shrinkTreeFrom (oppositeDirection dir) z
expandTreeTowards R (t, LeftCrumb s r:cs)
  | axis s == Vertical = Just (t, LeftCrumb (increaseRatio s resizeDiff) r:cs)
expandTreeTowards L (t, RightCrumb s l:cs)
  | axis s == Vertical = Just (t, RightCrumb (increaseRatio s (-resizeDiff)) l:cs)
expandTreeTowards D (t, LeftCrumb s r:cs)
  | axis s == Horizontal = Just (t, LeftCrumb (increaseRatio s resizeDiff) r:cs)
expandTreeTowards U (t, RightCrumb s l:cs)
  | axis s == Horizontal = Just (t, RightCrumb (increaseRatio s (-resizeDiff)) l:cs)
expandTreeTowards dir z = goUp z >>= expandTreeTowards dir

shrinkTreeFrom :: Direction2D -> Zipper Split -> Maybe (Zipper Split)
shrinkTreeFrom _ z@(_, []) = Just z
shrinkTreeFrom R z@(_, LeftCrumb s _:_)
  | axis s == Vertical = Just z >>= goSibling >>= expandTreeTowards L
shrinkTreeFrom L z@(_, RightCrumb s _:_)
  | axis s == Vertical = Just z >>= goSibling >>= expandTreeTowards R
shrinkTreeFrom D z@(_, LeftCrumb s _:_)
  | axis s == Horizontal = Just z >>= goSibling >>= expandTreeTowards U
shrinkTreeFrom U z@(_, RightCrumb s _:_)
  | axis s == Horizontal = Just z >>= goSibling >>= expandTreeTowards D
shrinkTreeFrom dir z = goUp z >>= shrinkTreeFrom dir

-- Direction2D refers to which direction the divider should move.
autoSizeTree :: Direction2D -> Zipper Split -> Maybe (Zipper Split)
autoSizeTree _ z@(_, []) = Just z
autoSizeTree d z =
    Just z >>= getSplit (toAxis d) >>= resizeTree d

-- resizing once found the correct split. YOU MUST FIND THE RIGHT SPLIT FIRST.
resizeTree :: Direction2D -> Zipper Split -> Maybe (Zipper Split)
resizeTree _ z@(_, []) = Just z
resizeTree R z@(_, LeftCrumb _ _:_) =
  Just z >>= expandTreeTowards R
resizeTree L z@(_, LeftCrumb _ _:_) =
  Just z >>= shrinkTreeFrom    R
resizeTree U z@(_, LeftCrumb _ _:_) =
  Just z >>= shrinkTreeFrom    D
resizeTree D z@(_, LeftCrumb _ _:_) =
  Just z >>= expandTreeTowards D
resizeTree R z@(_, RightCrumb _ _:_) =
  Just z >>= shrinkTreeFrom    L
resizeTree L z@(_, RightCrumb _ _:_) =
  Just z >>= expandTreeTowards L
resizeTree U z@(_, RightCrumb _ _:_) =
  Just z >>= expandTreeTowards U
resizeTree D z@(_, RightCrumb _ _:_) =
  Just z >>= shrinkTreeFrom    U

getSplit :: Axis -> Zipper Split -> Maybe (Zipper Split)
getSplit _ (_, []) = Nothing
getSplit d z =
 do let fs = findSplit d z
    if isNothing fs
      then findClosest d z
      else fs

findClosest :: Axis -> Zipper Split -> Maybe (Zipper Split)
findClosest _ z@(_, []) = Just z
findClosest d z@(_, LeftCrumb s _:_)
  | axis s == d = Just z
findClosest d z@(_, RightCrumb s _:_)
  | axis s == d = Just z
findClosest d z = goUp z >>= findClosest d

findSplit :: Axis -> Zipper Split -> Maybe (Zipper Split)
findSplit _ (_, []) = Nothing
findSplit d z@(_, LeftCrumb s _:_)
  | axis s == d = Just z
findSplit d z = goUp z >>= findSplit d

resizeSplit :: Direction2D -> (Rational,Rational) -> Zipper Split -> Maybe (Zipper Split)
resizeSplit _ _ z@(_, []) = Just z
resizeSplit dir (xsc,ysc) z = case goToBorder dir z of
  Nothing -> Just z
  Just (t, crumb) -> Just $ case dir of
    R -> (t{value=sp{ratio=scaleRatio (ratio sp) xsc}}, crumb)
    D -> (t{value=sp{ratio=scaleRatio (ratio sp) ysc}}, crumb)
    L -> (t{value=sp{ratio=1-scaleRatio (1-ratio sp) xsc}}, crumb)
    U -> (t{value=sp{ratio=1-scaleRatio (1-ratio sp) ysc}}, crumb)
    where sp = value t
          scaleRatio r fac = min 0.9 $ max 0.1 $ r*fac

-- starting from a leaf, go to node representing a border of the according window
goToBorder :: Direction2D -> Zipper Split -> Maybe (Zipper Split)
goToBorder L z@(_, RightCrumb (Split Vertical _) _:_) = goUp z
goToBorder L z = goUp z >>= goToBorder L
goToBorder R z@(_, LeftCrumb  (Split Vertical _) _:_) = goUp z
goToBorder R z = goUp z >>= goToBorder R
goToBorder U z@(_, RightCrumb (Split Horizontal _) _:_) = goUp z
goToBorder U z = goUp z >>= goToBorder U
goToBorder D z@(_, LeftCrumb  (Split Horizontal _) _:_) = goUp z
goToBorder D z = goUp z >>= goToBorder D

-- takes a list of indices and numerates the leaves of a given tree
numerate :: [Int] -> Tree a -> Tree a
numerate ns t = snd $ num ns t
  where num (n:nns) (Leaf _) = (nns, Leaf n)
        num [] (Leaf _) = ([], Leaf 0)
        num n (Node s l r) = (n'', Node s nl nr)
          where (n', nl)  = num n l
                (n'', nr) = num n' r

-- return values of leaves from left to right as list
flatten :: Tree a -> [Int]
flatten (Leaf n) = [n]
flatten (Node _ l r) = flatten l++flatten r

-- adjust ratios to make window areas equal
equalize :: Zipper Split -> Maybe (Zipper Split)
equalize (t, cs) = Just (eql t, cs)
  where eql (Leaf n) = Leaf n
        eql n@(Node s l r) = Node s{ratio=fromIntegral (numLeaves l) % fromIntegral (numLeaves n)}
                                  (eql l) (eql r)

-- generate a symmetrical balanced tree for n leaves from given tree, preserving leaf labels
balancedTree :: Zipper Split -> Maybe (Zipper Split)
balancedTree (t, cs) =  Just (numerate (flatten t) $ balanced (numLeaves t), cs)
  where balanced 1 = Leaf 0
        balanced 2 = Node (Split Horizontal 0.5) (Leaf 0) (Leaf 0)
        balanced m = Node (Split Horizontal 0.5) (balanced (m`div`2)) (balanced (m-m`div`2))

-- attempt to rotate splits optimally in order choose more quad-like rects
optimizeOrientation :: Rectangle -> Zipper Split -> Maybe (Zipper Split)
optimizeOrientation rct (t, cs) = Just (opt t rct, cs)
  where opt (Leaf v) _ = Leaf v
        opt (Node sp l r) rect = Node sp' (opt l lrect) (opt r rrect)
         where (Rectangle _ _ w1 h1,Rectangle _ _ w2 h2) = split (axis sp) (ratio sp) rect
               (Rectangle _ _ w3 h3,Rectangle _ _ w4 h4) = split (axis $ oppositeSplit sp) (ratio sp) rect
               f w h = if w > h then w'/h' else h'/w' where (w',h') = (fromIntegral w :: Double, fromIntegral h :: Double)
               wratio = min (f w1 h1) (f w2 h2)
               wratio' = min (f w3 h3) (f w4 h4)
               sp' = if wratio<wratio' then sp else oppositeSplit sp
               (lrect, rrect) = split (axis sp') (ratio sp') rect


-- initially focused leaf, path from root to selected node, window ids of borders highlighting the selection
data NodeRef = NodeRef { refLeaf :: Int, refPath :: [Direction2D], refWins :: [Window] } deriving (Show,Read,Eq)
noRef :: NodeRef
noRef = NodeRef (-1) [] []

goToNode :: NodeRef -> Zipper a -> Maybe (Zipper a)
goToNode (NodeRef _ dirs _) z = foldM gofun z dirs
  where gofun z' L = goLeft z'
        gofun z' R = goRight z'
        gofun _ _ = Nothing

toNodeRef :: Int -> Maybe (Zipper Split) -> NodeRef
toNodeRef _ Nothing = noRef
toNodeRef l (Just (_, cs)) = NodeRef l (reverse $ map crumbToDir cs) []
  where crumbToDir (LeftCrumb _ _) = L
        crumbToDir (RightCrumb _ _) = R

-- returns the leaf a noderef is leading to, if any
nodeRefToLeaf :: NodeRef -> Maybe (Zipper a) -> Maybe Int
nodeRefToLeaf n (Just z) = case goToNode n z of
  Just (Leaf l, _) -> Just l
  Just (Node _ _ _, _) -> Nothing
  Nothing -> Nothing
nodeRefToLeaf _ Nothing = Nothing

leafToNodeRef :: Int -> BinarySpacePartition a -> NodeRef
leafToNodeRef l b = toNodeRef l (makeZipper b >>= goToNthLeaf l)

data BinarySpacePartition a = BinarySpacePartition { getOldRects :: [(Window,Rectangle)]
                                                   , getFocusedNode :: NodeRef
                                                   , getSelectedNode :: NodeRef
                                                   , getTree :: Maybe (Tree Split) } deriving (Show, Read,Eq)

-- | an empty BinarySpacePartition to use as a default for adding windows to.
emptyBSP :: BinarySpacePartition a
emptyBSP = BinarySpacePartition [] noRef noRef Nothing

makeBSP :: Tree Split -> BinarySpacePartition a
makeBSP = BinarySpacePartition [] noRef noRef . Just

makeZipper :: BinarySpacePartition a -> Maybe (Zipper Split)
makeZipper (BinarySpacePartition _ _ _ Nothing) = Nothing
makeZipper (BinarySpacePartition _ _ _ (Just t)) = Just . toZipper $ t

size :: BinarySpacePartition a -> Int
size = maybe 0 numLeaves . getTree

zipperToBinarySpacePartition :: Maybe (Zipper Split) -> BinarySpacePartition b
zipperToBinarySpacePartition Nothing = emptyBSP
zipperToBinarySpacePartition (Just z) = BinarySpacePartition [] noRef noRef . Just . toTree . top $ z

rectangles :: BinarySpacePartition a -> Rectangle -> [Rectangle]
rectangles (BinarySpacePartition _ _ _ Nothing) _ = []
rectangles (BinarySpacePartition _ _ _ (Just (Leaf _))) rootRect = [rootRect]
rectangles (BinarySpacePartition _ _ _ (Just node)) rootRect =
    rectangles (makeBSP . left $ node) leftBox ++
    rectangles (makeBSP . right $ node) rightBox
    where (leftBox, rightBox) = split (axis info) (ratio info) rootRect
          info = value node

getNodeRect :: BinarySpacePartition a -> Rectangle -> NodeRef -> Rectangle
getNodeRect b r n = fromMaybe (Rectangle 0 0 1 1) (makeZipper b >>= goToNode n >>= getRect [])
  where getRect ls (_, []) = Just $ foldl (\r' (s,f) -> f $ split' s r') r ls
        getRect ls z@(_, LeftCrumb s _:_) = goUp z >>= getRect ((s,fst):ls)
        getRect ls z@(_, RightCrumb s _:_) = goUp z >>= getRect ((s,snd):ls)
        split' s = split (axis s) (ratio s)

doToNth :: (Zipper Split -> Maybe (Zipper Split)) -> BinarySpacePartition a -> BinarySpacePartition a
doToNth f b = b{getTree=getTree $ zipperToBinarySpacePartition $ makeZipper b >>= goToNode (getFocusedNode b) >>= f}

splitNth :: BinarySpacePartition a -> BinarySpacePartition a
splitNth (BinarySpacePartition _ _ _ Nothing) = makeBSP (Leaf 0)
splitNth b = doToNth splitCurrent b

removeNth :: BinarySpacePartition a -> BinarySpacePartition a
removeNth (BinarySpacePartition _ _ _ Nothing) = emptyBSP
removeNth (BinarySpacePartition _ _ _ (Just (Leaf _))) = emptyBSP
removeNth b = doToNth removeCurrent b

rotateNth :: BinarySpacePartition a -> BinarySpacePartition a
rotateNth (BinarySpacePartition _ _ _ Nothing) = emptyBSP
rotateNth b@(BinarySpacePartition _ _ _ (Just (Leaf _))) = b
rotateNth b = doToNth rotateCurrent b

swapNth :: BinarySpacePartition a -> BinarySpacePartition a
swapNth (BinarySpacePartition _ _ _ Nothing) = emptyBSP
swapNth b@(BinarySpacePartition _ _ _ (Just (Leaf _))) = b
swapNth b = doToNth swapCurrent b

splitShiftNth :: Direction1D -> BinarySpacePartition a -> BinarySpacePartition a
splitShiftNth _ (BinarySpacePartition _ _ _ Nothing) = emptyBSP
splitShiftNth _ b@(BinarySpacePartition _ _ _ (Just (Leaf _))) = b
splitShiftNth Prev b = doToNth splitShiftLeftCurrent b
splitShiftNth Next b = doToNth splitShiftRightCurrent b

growNthTowards :: Direction2D -> BinarySpacePartition a -> BinarySpacePartition a
growNthTowards _ (BinarySpacePartition _ _ _ Nothing) = emptyBSP
growNthTowards _ b@(BinarySpacePartition _ _ _ (Just (Leaf _))) = b
growNthTowards dir b = doToNth (expandTreeTowards dir) b

shrinkNthFrom :: Direction2D -> BinarySpacePartition a -> BinarySpacePartition a
shrinkNthFrom _ (BinarySpacePartition _ _ _ Nothing)= emptyBSP
shrinkNthFrom _ b@(BinarySpacePartition _ _ _ (Just (Leaf _))) = b
shrinkNthFrom dir b = doToNth (shrinkTreeFrom dir) b

autoSizeNth :: Direction2D -> BinarySpacePartition a -> BinarySpacePartition a
autoSizeNth _ (BinarySpacePartition _ _ _ Nothing) = emptyBSP
autoSizeNth _ b@(BinarySpacePartition _ _ _ (Just (Leaf _))) = b
autoSizeNth dir b = doToNth (autoSizeTree dir) b

resizeSplitNth :: Direction2D -> (Rational,Rational) -> BinarySpacePartition a -> BinarySpacePartition a
resizeSplitNth _ _ (BinarySpacePartition _ _ _ Nothing) = emptyBSP
resizeSplitNth _ _ b@(BinarySpacePartition _ _ _ (Just (Leaf _))) = b
resizeSplitNth dir sc b = doToNth (resizeSplit dir sc) b

-- rotate tree left or right around parent of nth leaf
rotateTreeNth :: Direction2D -> BinarySpacePartition a -> BinarySpacePartition a
rotateTreeNth _ (BinarySpacePartition _ _ _ Nothing) = emptyBSP
rotateTreeNth U b = b
rotateTreeNth D b = b
rotateTreeNth dir b@(BinarySpacePartition _ _ _ (Just _)) =
  doToNth (\t -> case goUp t of
                Nothing     -> Just t
                Just (t', c) -> Just (rotTree dir t', c)) b

equalizeNth :: BinarySpacePartition a -> BinarySpacePartition a
equalizeNth (BinarySpacePartition _ _ _ Nothing) = emptyBSP
equalizeNth b@(BinarySpacePartition _ _ _ (Just (Leaf _))) = b
equalizeNth b = doToNth equalize b

rebalanceNth :: BinarySpacePartition a -> Rectangle -> BinarySpacePartition a
rebalanceNth (BinarySpacePartition _ _ _ Nothing) _ = emptyBSP
rebalanceNth b@(BinarySpacePartition _ _ _ (Just (Leaf _))) _ = b
rebalanceNth b r = doToNth (balancedTree >=> optimizeOrientation r) b

flattenLeaves :: BinarySpacePartition a -> [Int]
flattenLeaves (BinarySpacePartition _ _ _ Nothing) = []
flattenLeaves (BinarySpacePartition _ _ _ (Just t)) = flatten t

-- we do this before an action to look afterwards which leaves moved where
numerateLeaves :: BinarySpacePartition a -> BinarySpacePartition a
numerateLeaves b@(BinarySpacePartition _ _ _ Nothing) = b
numerateLeaves b@(BinarySpacePartition _ _ _ (Just t)) = b{getTree=Just $ numerate ns t}
  where ns = [0..(numLeaves t-1)]

-- if there is a selected and focused node and the focused is not a part of selected,
-- move selected node to be a child of focused node
moveNode :: BinarySpacePartition a -> BinarySpacePartition a
moveNode b@(BinarySpacePartition _ (NodeRef (-1) _ _) _ _) = b
moveNode b@(BinarySpacePartition _ _ (NodeRef (-1) _ _) _) = b
moveNode b@(BinarySpacePartition _ _ _ Nothing) = b
moveNode b@(BinarySpacePartition _ f s (Just ot)) =
  case makeZipper b >>= goToNode s of
    Just (n, LeftCrumb _ t:cs)  -> b{getTree=Just $ insert n $ top (t, cs)}
    Just (n, RightCrumb _ t:cs) -> b{getTree=Just $ insert n $ top (t, cs)}
    _ -> b
  where insert t z = case goToNode f z of
          Nothing -> ot --return original tree (abort)
          Just (n, c:cs) -> toTree (Node (Split (oppositeAxis . axis . parentVal $ c) 0.5) t n, c:cs)
          Just (n, []) -> toTree (Node (Split Vertical 0.5) t n, [])

------------------------------------------

-- returns index of focused window or 0 for empty stack
index :: W.Stack a -> Int
index s = case toIndex (Just s) of
            (_, Nothing) -> 0
            (_, Just int) -> int

--move windows to new positions according to tree transformations, keeping focus on originally focused window
--CAREFUL here! introduce a bug here and have fun debugging as your windows start to disappear or explode
adjustStack :: Maybe (W.Stack Window)  --original stack
            -> Maybe (W.Stack Window)  --stack without floating windows
            -> [Window]                --just floating windows of this WS
            -> Maybe (BinarySpacePartition Window) -- Tree with numbered leaves telling what to move where
            -> Maybe (W.Stack Window)  --resulting stack
adjustStack orig Nothing _ _ = orig    --no new stack -> no changes
adjustStack orig _ _ Nothing = orig    --empty tree   -> no changes
adjustStack orig s fw (Just b) =
 if length ls<length ws then orig      --less leaves than non-floating windows -> tree incomplete, no changes
 else fromIndex ws' fid'
 where ws' = mapMaybe (`M.lookup` wsmap) ls ++ fw
       fid' = fromMaybe 0 $ elemIndex focused ws'
       wsmap = M.fromList $ zip [0..] ws -- map: old index in list -> window
       ls = flattenLeaves b              -- get new index ordering from tree
       (ws,fid) = toIndex s
       focused = ws !! fromMaybe 0 fid

--replace the window stack of the managed workspace with our modified stack
replaceStack :: Maybe (W.Stack Window) -> X ()
replaceStack s = do
  st <- get
  let wset = windowset st
      cur  = W.current wset
      wsp  = W.workspace cur
  put st{windowset=wset{W.current=cur{W.workspace=wsp{W.stack=s}}}}

replaceFloating :: M.Map Window W.RationalRect -> X ()
replaceFloating wsm = do
  st <- get
  let wset = windowset st
  put st{windowset=wset{W.floating=wsm}}

-- some helpers to filter windows
--
getFloating :: X [Window]
getFloating = (M.keys . W.floating) <$> gets windowset -- all floating windows

getStackSet :: X (Maybe (W.Stack Window))
getStackSet = (W.stack . W.workspace . W.current) <$> gets windowset -- windows on this WS (with floating)

getScreenRect :: X Rectangle
getScreenRect = (screenRect . W.screenDetail . W.current) <$> gets windowset

withoutFloating :: [Window] -> Maybe (W.Stack Window) -> Maybe (W.Stack Window)
withoutFloating fs = maybe Nothing (unfloat fs)

-- ignore messages if current focus is on floating window, otherwise return stack without floating
unfloat :: [Window] -> W.Stack Window -> Maybe (W.Stack Window)
unfloat fs s = if W.focus s `elem` fs
      then Nothing
      else Just $ s{W.up = W.up s \\ fs, W.down = W.down s \\ fs}

instance LayoutClass BinarySpacePartition Window where
  doLayout b r s = do
    let b' = layout b
    b'' <- updateNodeRef b' (size b/=size b') r
    let rs = rectangles b'' r
        wrs = zip ws rs
    return (wrs, Just b''{getOldRects=wrs})
    where
      ws = W.integrate s
      l = length ws
      layout bsp
        | l == sz = bsp
        | l > sz = layout $ splitNth bsp
        | otherwise = layout $ removeNth bsp
        where sz = size bsp

  handleMessage b_orig m
   | Just msg@(SetGeometry _) <- fromMessage m = handleResize b msg
   | Just FocusParent <- fromMessage m = do
       let n = getFocusedNode b
       let n' = toNodeRef (refLeaf n) (makeZipper b >>= goToNode n >>= goUp)
       return $ Just b{getFocusedNode=n'{refWins=refWins n}}
   | Just SelectNode <- fromMessage m = do
       let n = getFocusedNode b
       let s = getSelectedNode b
       removeBorder $ refWins s
       let s' = if refLeaf n == refLeaf s && refPath n == refPath s
                then noRef else n{refWins=[]}
       return $ Just b{getSelectedNode=s'}
   | otherwise = do
       ws <- getStackSet
       fs <- getFloating
       r <- getScreenRect
       -- removeBorder $ refWins $ getSelectedNode b
       let lws = withoutFloating fs ws                                    -- tiled windows on WS
           lfs = maybe [] W.integrate ws \\ maybe [] W.integrate lws      -- untiled windows on WS
           b'  = handleMesg r                -- transform tree (concerns only tiled windows)
           ws' = adjustStack ws lws lfs b'   -- apply transformation to window stack, reintegrate floating wins
       replaceStack ws'
       return b'
    where handleMesg r = msum [ fmap resize        (fromMessage m)
                              , fmap rotate        (fromMessage m)
                              , fmap swap          (fromMessage m)
                              , fmap rotateTr      (fromMessage m)
                              , fmap (balanceTr r) (fromMessage m)
                              , fmap move          (fromMessage m)
                              , fmap splitShift    (fromMessage m)
                              ]
          resize (ExpandTowards dir) = growNthTowards dir b
          resize (ShrinkFrom dir) = shrinkNthFrom dir b
          resize (MoveSplit dir) = autoSizeNth dir b
          rotate Rotate = resetFoc $ rotateNth b
          swap Swap = resetFoc $ swapNth b
          rotateTr RotateL = resetFoc $ rotateTreeNth L b
          rotateTr RotateR = resetFoc $ rotateTreeNth R b
          balanceTr _ Equalize = resetFoc $ equalizeNth b
          balanceTr r Balance  = resetFoc $ rebalanceNth b r
          move MoveNode = resetFoc $ moveNode b
          move SelectNode = b --should not happen here, is done above, as we need X monad
          splitShift (SplitShift dir) = resetFoc $ splitShiftNth dir b

          b = numerateLeaves b_orig
          resetFoc bsp = bsp{getFocusedNode=(getFocusedNode bsp){refLeaf=(-1)}
                            ,getSelectedNode=(getSelectedNode bsp){refLeaf=(-1)}}

  description _  = "BSP"

-- React to SetGeometry message to work with BorderResize/MouseResize
handleResize :: BinarySpacePartition Window -> WindowArrangerMsg -> X (Maybe (BinarySpacePartition Window))
handleResize b (SetGeometry newrect@(Rectangle _ _ w h)) = do
  ws <- getStackSet
  fs <- getFloating
  case W.focus <$> ws of
    Nothing -> return Nothing
    Just win -> do
      (_,_,_,_,_,mx,my,_) <- withDisplay (\d -> io $ queryPointer d win)
      let oldrect@(Rectangle _ _ ow oh) = fromMaybe (Rectangle 0 0 0 0) $ lookup win $ getOldRects b
      let (xsc,ysc)   = (fi w % fi ow, fi h % fi oh)
          (xsc',ysc') = (rough xsc, rough ysc)
          dirs = changedDirs oldrect newrect (fi mx,fi my)
          n = elemIndex win $ maybe [] W.integrate $ withoutFloating fs ws
      -- unless (isNothing dir) $ debug $
      --       show (fi x-fi ox,fi y-fi oy) ++ show (fi w-fi ow,fi h-fi oh)
      --       ++ show dir ++ " " ++ show win ++ " " ++ show (mx,my)
      return $ case n of
                Just _ -> Just $ foldl' (\b' d -> resizeSplitNth d (xsc',ysc') b') b dirs
                Nothing -> Nothing --focused window is floating -> ignore
  where rough v = min 1.5 $ max 0.75 v -- extreme scale factors are forbidden
handleResize _ _ = return Nothing

-- find out which borders have been pulled. We need the old and new rects and the mouse coordinates
changedDirs :: Rectangle -> Rectangle -> (Int,Int) -> [Direction2D]
changedDirs (Rectangle _ _ ow oh) (Rectangle _ _ w h) (mx,my) = catMaybes [lr, ud]
 where lr = if ow==w then Nothing
            else Just (if (fi mx :: Double) >  (fi ow :: Double)/2 then R else L)
       ud = if oh==h then Nothing
            else Just (if (fi my :: Double) > (fi oh :: Double)/2 then D else U)

-- node focus border helpers
----------------------------
updateNodeRef :: BinarySpacePartition Window -> Bool -> Rectangle -> X (BinarySpacePartition Window)
updateNodeRef b force r = do
    let n = getFocusedNode b
    let s = getSelectedNode b
    removeBorder (refWins n++refWins s)
    l <- getCurrFocused
    b' <- if refLeaf n /= l || refLeaf n == (-1) || force
            then return b{getFocusedNode=leafToNodeRef l b}
            else return b
    b'' <- if force then return b'{getSelectedNode=noRef} else return b'
    renderBorders r b''
  where getCurrFocused = maybe 0 index <$> (withoutFloating <$> getFloating <*> getStackSet)

-- create border around focused node if necessary
renderBorders :: Rectangle -> BinarySpacePartition a -> X (BinarySpacePartition a)
renderBorders r b = do
  let l = nodeRefToLeaf (getFocusedNode b) $ makeZipper b
  wssel <- if refLeaf (getSelectedNode b)/=(-1)
           then createBorder (getNodeRect b r (getSelectedNode b)) $ Just "#00ff00"
           else return []
  let b' = b{getSelectedNode=(getSelectedNode b){refWins=wssel}}
  if refLeaf (getFocusedNode b')==(-1) || isJust l || size b'<2 then return b'
    else do
      ws' <- createBorder (getNodeRect b' r (getFocusedNode b')) Nothing
      return b'{getFocusedNode=(getFocusedNode b'){refWins=ws'}}

-- create a window for each border line, show, add into stack and set floating
createBorder :: Rectangle -> Maybe String -> X [Window]
createBorder (Rectangle wx wy ww wh) c = do
  bw <- asks (borderWidth.config)
  bc <- case c of
         Nothing -> asks (focusedBorderColor.config)
         Just s -> return s
  let rects = [ Rectangle wx wy ww (fi bw)
              , Rectangle wx wy (fi bw) wh
              , Rectangle wx (wy+fi wh-fi bw) ww (fi bw)
              , Rectangle (wx+fi ww-fi bw) wy (fi bw) wh
              ]
  ws <- mapM (\r -> createNewWindow r Nothing bc False) rects
  showWindows ws
  maybe Nothing (\s -> Just s{W.down=W.down s ++ ws}) <$> getStackSet >>= replaceStack
  M.union (M.fromList $ zip ws $ map toRR rects) . W.floating . windowset <$> get >>= replaceFloating
  modify (\s -> s{mapped=mapped s `S.union` S.fromList ws})
  -- show <$> mapM isClient ws >>= debug
  return ws
  where toRR (Rectangle x y w h) = W.RationalRect (fi x) (fi y) (fi w) (fi h)

-- remove border line windows from stack + floating, kill
removeBorder :: [Window] -> X ()
removeBorder ws = do
  modify (\s -> s{mapped = mapped s `S.difference` S.fromList ws})
  flip (foldl (flip M.delete)) ws . W.floating . windowset <$> get >>= replaceFloating
  maybe Nothing (\s -> Just s{W.down=W.down s \\ ws}) <$> getStackSet >>= replaceStack
  deleteWindows ws
