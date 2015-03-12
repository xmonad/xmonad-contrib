{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, DeriveDataTypeable #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.BinarySpacePartition
-- Copyright   :  (c) 2013 Ben Weitzman    <benweitzman@gmail.com>
--                    2015 Anton Pirogov   <anton.pirogov@gmail.com>
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
  , TreeRotate(..)
  , TreeBalance(..)
  , FocusParent(..)
  , Direction2D(..)
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
import Control.Applicative
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
-- > , ((modm .|. altMask,               xK_l     ), sendMessage $ ExpandTowards R)
-- > , ((modm .|. altMask,               xK_h     ), sendMessage $ ExpandTowards L)
-- > , ((modm .|. altMask,               xK_j     ), sendMessage $ ExpandTowards D)
-- > , ((modm .|. altMask,               xK_k     ), sendMessage $ ExpandTowards U)
-- > , ((modm .|. altMask .|. ctrlMask , xK_l     ), sendMessage $ ShrinkFrom R)
-- > , ((modm .|. altMask .|. ctrlMask , xK_h     ), sendMessage $ ShrinkFrom L)
-- > , ((modm .|. altMask .|. ctrlMask , xK_j     ), sendMessage $ ShrinkFrom D)
-- > , ((modm .|. altMask .|. ctrlMask , xK_k     ), sendMessage $ ShrinkFrom U)
-- > , ((modm,                           xK_r     ), sendMessage Rotate)
-- > , ((modm,                           xK_s     ), sendMessage Swap)
-- > , ((modm,                           xK_n     ), sendMessage FocusParent)
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
-- > , ("M-M1-s",         sendMessage $ Rotate) ]
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

-- |Message to select the parent node instead of the leaf
data FocusParent = FocusParent deriving Typeable
instance Message FocusParent

data Axis = Horizontal | Vertical deriving (Show, Read, Eq)

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
rotTree dir (Leaf n) = (Leaf n)
rotTree R n@(Node _ (Leaf _) _) = n
rotTree L n@(Node _ _ (Leaf _)) = n
rotTree R (Node sp (Node sp2 l2 r2) r) = Node sp2 l2 (Node sp r2 r)
rotTree L (Node sp l (Node sp2 l2 r2)) = Node sp2 (Node sp l l2) r2


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

goToFocusedLocation :: (Int,Int,[Window]) -> Zipper a -> Maybe (Zipper a)
goToFocusedLocation (l,n,_) z = goToNthLeaf l z >>= goUpN n
  where goUpN 0 b = return b
        goUpN n b = goUp b >>= goUpN (n-1)

splitCurrentLeaf :: Zipper Split -> Maybe (Zipper Split)
splitCurrentLeaf (Leaf _, []) = Just (Node (Split Vertical 0.5) (Leaf 0) (Leaf 0), [])
splitCurrentLeaf (Leaf _, crumb:cs) = Just (Node (Split (oppositeAxis . axis . parentVal $ crumb) 0.5) (Leaf 0) (Leaf 0), crumb:cs)
splitCurrentLeaf _ = Nothing

removeCurrentLeaf :: Zipper a -> Maybe (Zipper a)
removeCurrentLeaf (Leaf _, []) = Nothing
removeCurrentLeaf (Leaf _, LeftCrumb _ r:cs) = Just (r, cs)
removeCurrentLeaf (Leaf _, RightCrumb _ l:cs) = Just (l, cs)
removeCurrentLeaf _ = Nothing

rotateCurrent :: Zipper Split -> Maybe (Zipper Split)
rotateCurrent l@(Leaf _, []) = Just l
rotateCurrent (n, c:cs) = Just (n, modifyParentVal oppositeSplit c:cs)
rotateCurrent _ = Nothing

swapCurrent :: Zipper a -> Maybe (Zipper a)
swapCurrent l@(Leaf _, []) = Just l
swapCurrent (n, c:cs) = Just (n, swapCrumb c:cs)
swapCurrent _ = Nothing

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
goToBorder L z@(_, RightCrumb (Split Vertical _) l:cs) = goUp z
goToBorder L z = goUp z >>= goToBorder L
goToBorder R z@(_, LeftCrumb  (Split Vertical _) r:cs) = goUp z
goToBorder R z = goUp z >>= goToBorder R
goToBorder U z@(_, RightCrumb (Split Horizontal _) l:cs) = goUp z
goToBorder U z = goUp z >>= goToBorder U
goToBorder D z@(_, LeftCrumb  (Split Horizontal _) r:cs) = goUp z
goToBorder D z = goUp z >>= goToBorder D


data BinarySpacePartition a = BinarySpacePartition { getOldRects :: [(Window,Rectangle)]
                                                   , getFocusedNode :: (Int,Int,[Window]) -- leaf, steps up,deco
                                                   , getTree :: Maybe (Tree Split) } deriving (Show, Read)

-- | an empty BinarySpacePartition to use as a default for adding windows to.
emptyBSP :: BinarySpacePartition a
emptyBSP = BinarySpacePartition [] ((-1),0,[]) Nothing

makeBSP :: Tree Split -> BinarySpacePartition a
makeBSP = BinarySpacePartition [] ((-1),0,[]) . Just

makeZipper :: BinarySpacePartition a -> Maybe (Zipper Split)
makeZipper (BinarySpacePartition _ _ Nothing) = Nothing
makeZipper (BinarySpacePartition _ _ (Just t)) = Just . toZipper $ t

size :: BinarySpacePartition a -> Int
size = maybe 0 numLeaves . getTree

zipperToBinarySpacePartition :: Maybe (Zipper Split) -> BinarySpacePartition b
zipperToBinarySpacePartition Nothing = emptyBSP
zipperToBinarySpacePartition (Just z) = BinarySpacePartition [] ((-1),0,[]) . Just . toTree . top $ z

rectangles :: BinarySpacePartition a -> Rectangle -> [Rectangle]
rectangles (BinarySpacePartition _ _ Nothing) _ = []
rectangles (BinarySpacePartition _ _ (Just (Leaf _))) rootRect = [rootRect]
rectangles (BinarySpacePartition _ _ (Just node)) rootRect =
    rectangles (makeBSP . left $ node) leftBox ++
    rectangles (makeBSP . right $ node) rightBox
    where (leftBox, rightBox) = split (axis info) (ratio info) rootRect
          info = value node

getNodeRect :: BinarySpacePartition a -> Rectangle -> (Int,Int) -> Rectangle
getNodeRect b r (l,n) = fromMaybe (Rectangle 0 0 1 1)
                      $ (makeZipper b >>= goToFocusedLocation (l,n,[]) >>= getRect [])
  where getRect ls z@(n, []) = Just $ foldl (\r' (s,f) -> f $ split' s r') r ls
        getRect ls z@(n, LeftCrumb s t:cs) = goUp z >>= getRect ((s,fst):ls)
        getRect ls z@(n, RightCrumb s t:cs) = goUp z >>= getRect ((s,snd):ls)
        split' s = split (axis s) (ratio s)

doToNth :: (Zipper Split -> Maybe (Zipper Split)) -> BinarySpacePartition a -> Int -> BinarySpacePartition a
doToNth f b n = zipperToBinarySpacePartition $ makeZipper b >>= goToFocusedLocation (getFocusedNode b) >>= f

splitNth :: BinarySpacePartition a -> Int -> BinarySpacePartition a
splitNth (BinarySpacePartition _ _ Nothing) _ = makeBSP (Leaf 0)
splitNth b n = doToNth splitCurrentLeaf b n

removeNth :: BinarySpacePartition a -> Int -> BinarySpacePartition a
removeNth (BinarySpacePartition _ _ Nothing) _ = emptyBSP
removeNth (BinarySpacePartition _ _ (Just (Leaf _))) _ = emptyBSP
removeNth b n = doToNth removeCurrentLeaf b n

rotateNth :: BinarySpacePartition a -> Int -> BinarySpacePartition a
rotateNth (BinarySpacePartition _ _ Nothing) _ = emptyBSP
rotateNth b@(BinarySpacePartition _ _ (Just (Leaf _))) _ = b
rotateNth b n = doToNth rotateCurrent b n

swapNth :: BinarySpacePartition a -> Int -> BinarySpacePartition a
swapNth (BinarySpacePartition _ _ Nothing) _ = emptyBSP
swapNth b@(BinarySpacePartition _ _ (Just (Leaf _))) _ = b
swapNth b n = doToNth swapCurrent b n

growNthTowards :: Direction2D -> BinarySpacePartition a -> Int -> BinarySpacePartition a
growNthTowards _ (BinarySpacePartition _ _ Nothing) _ = emptyBSP
growNthTowards _ b@(BinarySpacePartition _ _ (Just (Leaf _))) _ = b
growNthTowards dir b n = doToNth (expandTreeTowards dir) b n

shrinkNthFrom :: Direction2D -> BinarySpacePartition a -> Int -> BinarySpacePartition a
shrinkNthFrom _ (BinarySpacePartition _ _ Nothing) _ = emptyBSP
shrinkNthFrom _ b@(BinarySpacePartition _ _ (Just (Leaf _))) _ = b
shrinkNthFrom dir b n = doToNth (shrinkTreeFrom dir) b n

autoSizeNth :: Direction2D -> BinarySpacePartition a -> Int -> BinarySpacePartition a
autoSizeNth _ (BinarySpacePartition _ _ Nothing) _ = emptyBSP
autoSizeNth _ b@(BinarySpacePartition _ _ (Just (Leaf _))) _ = b
autoSizeNth dir b n = doToNth (autoSizeTree dir) b n

resizeSplitNth :: Direction2D -> (Rational,Rational) -> BinarySpacePartition a -> Int -> BinarySpacePartition a
resizeSplitNth _ _ (BinarySpacePartition _ _ Nothing) _ = emptyBSP
resizeSplitNth _ _ b@(BinarySpacePartition _ _ (Just (Leaf _))) _ = b
resizeSplitNth dir sc b n = doToNth (resizeSplit dir sc) b n

-- rotate tree left or right around parent of nth leaf
rotateTreeNth :: Direction2D -> BinarySpacePartition a -> Int -> BinarySpacePartition a
rotateTreeNth _ (BinarySpacePartition _ _ Nothing) _ = emptyBSP
rotateTreeNth U b _ = b
rotateTreeNth D b _ = b
rotateTreeNth dir b@(BinarySpacePartition _ _ (Just t)) n =
  doToNth (\t -> case goUp t of
                Nothing     -> Just t
                Just (t, c) -> Just (rotTree dir t, c)) b n

-- set the split ratios so that all windows have the same size, without changing tree itself
equalizeTree :: BinarySpacePartition a -> BinarySpacePartition a
equalizeTree (BinarySpacePartition _ _ Nothing) = emptyBSP
equalizeTree (BinarySpacePartition olr foc (Just t)) = BinarySpacePartition olr foc $ Just $ eql t
  where eql (Leaf n) = Leaf n
        eql n@(Node s l r) = Node s{ratio=fromIntegral (numLeaves l) % fromIntegral (numLeaves n)}
                                  (eql l) (eql r)

-- generate a symmetrical balanced tree for n leaves
balancedTree :: Int -> BinarySpacePartition a
balancedTree n =  numerateLeaves $ BinarySpacePartition [] ((-1),0,[]) $ Just $ balanced n
  where balanced 1 = Leaf 0
        balanced 2 = Node (Split Horizontal 0.5) (Leaf 0) (Leaf 0)
        balanced n = Node (Split Horizontal 0.5) (balanced (n`div`2)) (balanced (n-n`div`2))

-- attempt to rotate splits optimally in order choose more quad-like rects
optimizeOrientation :: Rectangle -> BinarySpacePartition a -> BinarySpacePartition a
optimizeOrientation r (BinarySpacePartition _ _ Nothing) = emptyBSP
optimizeOrientation r (BinarySpacePartition olr foc (Just t)) = BinarySpacePartition olr foc $ Just $ opt t r
  where opt (Leaf v) rect = (Leaf v)
        opt (Node sp l r) rect = Node sp' (opt l lrect) (opt r rrect)
         where (Rectangle _ _ w1 h1,Rectangle _ _ w2 h2) = split (axis sp) (ratio sp) rect
               (Rectangle _ _ w3 h3,Rectangle _ _ w4 h4) = split (axis $ oppositeSplit sp) (ratio sp) rect
               f w h = if w > h then w'/h' else h'/w' where (w',h') = (fromIntegral w, fromIntegral h)
               wratio = min (f w1 h1) (f w2 h2)
               wratio' = min (f w3 h3) (f w4 h4)
               sp' = if wratio<wratio' then sp else oppositeSplit sp
               (lrect, rrect) = split (axis sp') (ratio sp') rect

-- traverse and collect all leave numbers, left to right
flattenLeaves :: BinarySpacePartition a -> [Int]
flattenLeaves (BinarySpacePartition _ _ Nothing) = []
flattenLeaves (BinarySpacePartition _ _ (Just t)) = flatten t
 where flatten (Leaf n) = [n]
       flatten (Node _ l r) = flatten l++flatten r

-- we do this before an action to look afterwards which leaves moved where
numerateLeaves :: BinarySpacePartition a -> BinarySpacePartition a
numerateLeaves b@(BinarySpacePartition _ _ Nothing) = b
numerateLeaves b@(BinarySpacePartition olr foc (Just t)) = BinarySpacePartition olr foc . Just . snd $ numerate 0 t
  where numerate n (Leaf _) = (n+1, Leaf n)
        numerate n (Node s l r) = (n'', Node s nl nr)
          where (n', nl) = numerate n l
                (n'', nr) = numerate n' r

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
adjustStack orig Nothing _ _ = orig   --no new stack -> no changes
adjustStack orig _ _ Nothing = orig   --empty tree   -> no changes
adjustStack orig s fw (Just b) =
 if length ls<length ws then orig     --less leaves than non-floating windows -> tree incomplete, no changes
 else fromIndex ws' fid'
 where ws' = (mapMaybe ((flip M.lookup) wsmap) ls)++fw
       fid' = fromMaybe 0 $ elemIndex focused ws'
       wsmap = M.fromList $ zip [0..] ws -- map: old index in list -> window
       ls = flattenLeaves b             -- get new index ordering from tree
       (ws,fid) = toIndex s
       focused = ws !! (fromMaybe 0 $ fid)

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
getFloating = (M.keys . W.floating) <$> gets windowset -- all floating windows
getStackSet = (W.stack . W.workspace . W.current) <$> gets windowset -- windows on this WS (with floating)
withoutFloating fs = maybe Nothing (unfloat fs)
isFloating w = getFloating >>= (\fs -> return $ w `elem` fs)
getScreenRect = (screenRect . W.screenDetail . W.current) <$> gets windowset

-- ignore messages if current focus is on floating window, otherwise return stack without floating
unfloat :: [Window] -> W.Stack Window -> Maybe (W.Stack Window)
unfloat fs s = if W.focus s `elem` fs
      then Nothing
      else Just $ s{W.up = W.up s \\ fs, W.down = W.down s \\ fs}

instance LayoutClass BinarySpacePartition Window where
  doLayout b r s = do
    let b' = layout b
    b'' <- if size b /= size b' then clearBorder b' else updateBorder r b'
    -- when (getFocusedNode b/= getFocusedNode b'') $ debug $ show $ getFocusedNode b''

    let rs = rectangles b'' r
        wrs = zip ws rs
    return (wrs, Just b''{getOldRects=wrs,getFocusedNode=getFocusedNode b''})
    where
      ws = W.integrate s
      l = length ws
      n = index s
      layout bsp
        | l == count = bsp
        | l > count = layout $ splitNth bsp n
        | otherwise = layout $ removeNth bsp n
        where count = size bsp

  handleMessage b_orig m
   | Just FocusParent <- fromMessage m = focusParent b
   | Just msg@(SetGeometry _) <- fromMessage m = handleResize b msg >>= return . updateNodeFocus
   | otherwise = do
       ws <- getStackSet
       fs <- getFloating
       r <- getScreenRect
       let lws = withoutFloating fs ws                                    -- tiled windows on WS
           lfs = (maybe [] W.integrate ws) \\ (maybe [] W.integrate lws)  -- untiled windows on WS
           b'  = lws >>= handleMesg r         -- transform tree (concerns only tiled windows)
           ws' = adjustStack ws lws lfs b'   -- apply transformation to window stack, reintegrate floating wins
       replaceStack ws'
       return $ updateNodeFocus b'
    where handleMesg r s = msum [fmap (`rotate` s)   (fromMessage m)
                                ,fmap (`resize` s)   (fromMessage m)
                                ,fmap (`swap` s)     (fromMessage m)
                                ,fmap (`rotateTr` s) (fromMessage m)
                                ,fmap (balanceTr r)  (fromMessage m)
                              ]

          updateNodeFocus = maybe Nothing (\bsp -> Just $ bsp{getFocusedNode=clr $ getFocusedNode b_orig})
            where clr (_,_,ws) = ((-1),0,ws)

          b = numerateLeaves b_orig

          rotate Rotate s = rotateNth b $ index s
          swap Swap s = swapNth b $ index s
          resize (ExpandTowards dir) s = growNthTowards dir b $ index s
          resize (ShrinkFrom dir) s = shrinkNthFrom dir b $ index s
          resize (MoveSplit dir) s = autoSizeNth dir b $ index s
          rotateTr RotateL s = rotateTreeNth L b $ index s
          rotateTr RotateR s = rotateTreeNth R b $ index s
          balanceTr r Equalize = equalizeTree b
          balanceTr r Balance = optimizeOrientation r $ balancedTree (size b)

  description _  = "BSP"

-- React to SetGeometry message to work with BorderResize/MouseResize
handleResize :: BinarySpacePartition Window -> WindowArrangerMsg -> X (Maybe (BinarySpacePartition Window))
handleResize b (SetGeometry newrect@(Rectangle x y w h)) = do
  ws <- getStackSet
  fs <- getFloating
  case W.focus <$> ws of
    Nothing -> return Nothing
    Just win -> do
      isfloat <- isFloating win
      (_,_,_,_,_,mx,my,_) <- withDisplay (\d -> (io $ queryPointer d win))
      let oldrect@(Rectangle ox oy ow oh) = fromMaybe (Rectangle 0 0 0 0) $ lookup win $ getOldRects b
      let (xsc,ysc)   = (fi w % fi ow, fi h % fi oh)
          (xsc',ysc') = (rough xsc, rough ysc)
          dirs = changedDirs oldrect newrect (fi mx,fi my)
          n = elemIndex win $ maybe [] W.integrate $ withoutFloating fs ws
      -- unless (isNothing dir) $ debug $
      --       show (fi x-fi ox,fi y-fi oy) ++ show (fi w-fi ow,fi h-fi oh)
      --       ++ show dir ++ " " ++ show win ++ " " ++ show (mx,my)
      return $ case n of
                Just n' -> Just $ foldl' (\b' d -> resizeSplitNth d (xsc',ysc') b' n') b dirs
                Nothing -> Nothing --focused window is floating -> ignore

  where rough v = min 1.5 $ max 0.75 v -- extreme scale factors are forbidden

-- find out which borders have been pulled. We need the old and new rects and the mouse coordinates
changedDirs :: Rectangle -> Rectangle -> (Int,Int) -> [Direction2D]
changedDirs (Rectangle ox oy ow oh) (Rectangle x y w h) (mx,my) = catMaybes [lr, ud]
 where lr = if ow==w then Nothing
            else Just (if fi mx > (fi ow)/2 then R else L)
       ud = if oh==h then Nothing
            else Just (if fi my > (fi oh)/2 then D else U)

-- move focus to next higher parent node of current focused leaf if possible, cyclic
focusParent :: BinarySpacePartition a -> X (Maybe (BinarySpacePartition a))
focusParent b = do
  foc <- maybe 0 index <$> (withoutFloating <$> getFloating <*> getStackSet)
  let (l,n,d) = getFocusedNode b
  return . Just $ if foc/= l then b{getFocusedNode=(foc,1,d)}
                            else b{getFocusedNode=upFocus (l,n,d)}
  -- debug $ "Focus Parent: "++(maybe "" (show.getFocusedNode) ret)
  where upFocus (l,n,d)
         | canFocus (l,n+1,d) = (l,n+1,d)
         | otherwise = (l,0,d)
        canFocus (l,n,d) = isJust $ makeZipper b >>= goToFocusedLocation (l,n+1,d)

-- "focus parent" border helpers

updateBorder :: Rectangle -> BinarySpacePartition a -> X (BinarySpacePartition a)
updateBorder r b = do
  foc <- maybe 0 index <$> (withoutFloating <$> getFloating <*> getStackSet)
  let (l,n,ws) = getFocusedNode b
  removeBorder ws
  if n==0 || foc/=l then return $ b{getFocusedNode=(foc,0,[])}
  else createBorder (getNodeRect b r (l,n)) Nothing >>= (\ws -> return $ b{getFocusedNode=(l,n,ws)})

clearBorder :: BinarySpacePartition a -> X (BinarySpacePartition a)
clearBorder b = do
  let (_,_,ws) = getFocusedNode b
  removeBorder ws
  return b{getFocusedNode=((-1),0,[])}

-- create a window for each border line, show, add into stack and set floating
createBorder r@(Rectangle wx wy ww wh) c = do
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
removeBorder ws = do
  modify (\s -> s{mapped = mapped s `S.difference` S.fromList ws})
  flip (foldl (flip M.delete)) ws . W.floating . windowset <$> get >>= replaceFloating
  maybe Nothing (\s -> Just s{W.down=W.down s \\ ws}) <$> getStackSet >>= replaceStack
  deleteWindows ws

debug str = spawn $ "echo \""++str++"\" >> /tmp/xdebug"

