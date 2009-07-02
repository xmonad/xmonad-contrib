{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
-----------------------------------------------------------------------------
-- |
-- Module       : XMonad.Actions.WorkspaceCursors
-- Copyright    : (c) 2009 Adam Vogt <vogt.adam@gmail.com>
-- License      : BSD
--
-- Maintainer   : Adam Vogt
-- Stability    : unstable
-- Portability  : portable
--
-- Generalizes plane to arbitrary dimensions.
-----------------------------------------------------------------------------

module XMonad.Actions.WorkspaceCursors
    (
    -- * Usage
    -- $usage
    toList
    ,focusDepth

    ,workspaceCursors

    ,modifyLayer
    ,makeCursors
    ,sampleCursors

    -- * Functions to pass to 'modifyLayer'
    ,focusNth'
    ,noWrapUp,noWrapDown
    ) where


import XMonad.Actions.FocusNth(focusNth')
import XMonad.Layout.LayoutModifier(LayoutModifier(handleMess,
                                                   redoLayout))
import XMonad(Typeable, Message, WorkspaceId, X, XState(windowset),
              fromMessage, sendMessage, windows, gets)
import Control.Applicative((<$>))
import Control.Monad(Monad(return, (>>=), (>>)), Functor(..),
                     guard, unless)
import Data.Foldable(Foldable(foldMap), toList)
import Data.Maybe(Maybe(Nothing), fromJust, listToMaybe)
import Data.Monoid(Monoid(mconcat, mappend))
import Data.Traversable(sequenceA)

import qualified XMonad.StackSet as W

-- $usage
--
-- Here is an example config:
--
-- > import XMonad
-- > import XMonad.Actions.WorkspaceCursors
-- > import XMonad.Config.Desktops
-- > import XMonad.Util.EZConfig
-- >
-- > main = do
-- >    xmonad $ additionalKeysP desktopConfig
-- >        { workspaces = toList sampleCurs
-- >        , layoutHook = workspaceCursors myCursors $ layoutHook desktopConfig
-- >        }
-- >        [("M-"++shift++[k], modifyLayer f depth)
-- >          | (f,shift) <- zip [W.focusUp',W.focusDown'] [[],"S-"]
-- >          , (depth,k) <- zip [1..focusDepth myCursors] "asdf"]
-- >
-- > myCursors = makeCursors $ map (map show) [[1..3],[1..3],[1..3],[1..9]]

-------------------------------------------------------------------------------

sampleCursors ::  Cursors String
sampleCursors = makeCursors $ map (map show) [[1..3::Int],[1..3],[1..9]]

makeCursors ::  [[String]] -> Cursors String
makeCursors (x:xs) = Prelude.foldr addDim (end x) xs
makeCursors [] = error "Cursors cannot be empty"

addDim ::  (Monoid a) => [a] -> Cursors a -> Cursors a
addDim prefixes prev = Cons . fromJust . W.differentiate
            $ map (\p -> fmap (p `mappend`) prev) prefixes

end :: [String] -> Cursors String
end = Cons . fromJust . W.differentiate . map End

data Cursors a
    = Cons (W.Stack (Cursors a))
    | End a deriving (Eq,Show,Read,Typeable)

instance Foldable Cursors where
    foldMap f (End x) = f x
    foldMap f (Cons st) = mconcat $ map (foldMap f) $ W.integrate st

instance Functor Cursors where
    fmap f (End a) = End $ f a
    fmap f (Cons (W.Stack x y z)) = Cons $ W.Stack (fmap f x) (fmap (fmap f) y) (fmap (fmap f) z)

changeFocus ::  (Cursors t -> Bool) -> Cursors t -> [Cursors t]
changeFocus p (Cons x) = chFocus p x >>= changeFocus p . Cons
changeFocus p x = guard (p x) >> return x

chFocus :: (a -> Bool) -> W.Stack a -> [W.Stack a]
chFocus p st = filter (p . W.focus) $ zipWith const (iterate W.focusDown' st) (W.integrate st)

getFocus ::  Cursors b -> b
getFocus (Cons x) = getFocus $ W.focus x
getFocus (End x) = x

focusTo ::  (Eq t) => t -> Cursors t -> Maybe (Cursors t)
focusTo x = listToMaybe . changeFocus ((x==) . getFocus)

-- | non-wrapping version of 'XMonad.StackSet.focusUp''
noWrapUp ::  W.Stack t -> W.Stack t
noWrapUp (W.Stack t (l:ls) rs) = W.Stack l ls (t:rs)
noWrapUp x@(W.Stack _ []   _ ) = x

-- | non-wrapping version of 'XMonad.StackSet.focusDown''
noWrapDown ::  W.Stack t -> W.Stack t
noWrapDown = reverseStack . noWrapUp . reverseStack
    where reverseStack (W.Stack t ls rs) = W.Stack t rs ls

focusDepth ::  Cursors t -> Int
focusDepth (Cons x) = 1 + focusDepth (W.focus x)
focusDepth (End  _) = 0

descend :: (W.Stack (Cursors a) -> W.Stack (Cursors a))-> Int-> Cursors a-> Cursors a
descend f 1 (Cons x) = Cons $ f x
descend f n (Cons x) | n > 1 = Cons $ descend f (pred n) `onFocus` x
descend _ _ x = x

onFocus ::  (a -> a) -> W.Stack a -> W.Stack a
onFocus f st = st { W.focus = f $ W.focus st }

modifyLayer :: (W.Stack (Cursors String) -> W.Stack (Cursors String)) -> Int -> X ()
modifyLayer f depth = modifyCursors (return . descend f depth)

modifyCursors ::  (Cursors String -> X (Cursors String)) -> X ()
modifyCursors = sendMessage . ChangeCursors

currentWs :: X WorkspaceId
currentWs = gets $ W.tag . W.workspace . W.current . windowset

data WorkspaceCursors a = WorkspaceCursors Bool (Cursors String) deriving (Typeable,Read,Show)

-- | WorkspaceCursors is implemented as a layout modifier, since that state is
-- serialized, and easily modified (with sendMessage)
workspaceCursors ::  Cursors String -> WorkspaceCursors a
workspaceCursors = WorkspaceCursors False

data ChangeCursors = ChangeCursors {
    unWrap :: Cursors String -> X (Cursors String)
    } deriving (Typeable)

instance Message ChangeCursors

instance LayoutModifier WorkspaceCursors a where
    redoLayout (WorkspaceCursors False cs) _ _ arrs = do
        cws <- currentWs
        return (arrs,do
            guard (getFocus cs /= cws)
            fmap (WorkspaceCursors True) $ focusTo cws cs)

    redoLayout (WorkspaceCursors _ cs) _ _ arrs = do
        cws <- currentWs
        -- redundant check to avoid switching workspaces
        unless (getFocus cs == cws) $ windows $ W.greedyView (getFocus cs)
        return (arrs,Nothing)

    handleMess (WorkspaceCursors prevMod cs) m =
        let wrap x = WorkspaceCursors (max prevMod (x /= cs)) x
        in sequenceA $ fmap wrap . ($ cs) . unWrap <$> fromMessage m
