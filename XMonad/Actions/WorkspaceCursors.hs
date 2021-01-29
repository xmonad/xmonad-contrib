{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
-----------------------------------------------------------------------------
-- |
-- Module       : XMonad.Actions.WorkspaceCursors
-- Copyright    : (c) 2009 Adam Vogt <vogt.adam@gmail.com>
-- License      : BSD
--
-- Maintainer   : Adam Vogt
-- Stability    : unstable
-- Portability  : unportable
--
-- Like "XMonad.Actions.Plane" for an arbitrary number of dimensions.
-----------------------------------------------------------------------------

module XMonad.Actions.WorkspaceCursors
    (
    -- * Usage
    -- $usage

     focusDepth
    ,makeCursors
    ,toList
    ,workspaceCursors

    ,WorkspaceCursors
    ,getFocus

    -- * Modifying the focus
    ,modifyLayer
    ,modifyLayer'
    ,shiftModifyLayer,shiftLayer

    -- * Functions to pass to 'modifyLayer'
    ,focusNth'
    ,noWrapUp,noWrapDown,

    -- * Todo
    -- $todo

    -- * Types
    Cursors,
    ) where

import qualified XMonad.StackSet as W

import XMonad.Actions.FocusNth(focusNth')
import XMonad.Layout.LayoutModifier(ModifiedLayout(..),
                                    LayoutModifier(handleMess, redoLayout))
import XMonad(Typeable, Message, WorkspaceId, X, XState(windowset),
              fromMessage, sendMessage, windows, gets)
import Control.Applicative (liftA2)
import Control.Monad((<=<), guard, when)
import Data.Foldable(toList)
import Data.Maybe(fromJust, listToMaybe)

-- $usage
--
-- Here is an example config:
--
-- > import XMonad
-- > import XMonad.Actions.WorkspaceCursors
-- > import XMonad.Hooks.DynamicLog
-- > import XMonad.Util.EZConfig
-- > import qualified XMonad.StackSet as W
-- >
-- > main = do
-- >     x <- xmobar conf
-- >     xmonad x
-- >
-- > conf = additionalKeysP def
-- >        { layoutHook = workspaceCursors myCursors $ layoutHook def
-- >        , workspaces = toList myCursors } $
-- >        [("M-"++shift++control++[k], f direction depth)
-- >          | (f,shift) <- zip [modifyLayer,shiftModifyLayer] ["","S-"]
-- >          , (direction,control) <- zip [W.focusUp',W.focusDown'] ["C-",""]
-- >          , (depth,k) <- zip (reverse [1..focusDepth myCursors]) "asdf"]
-- >        ++ moreKeybindings
-- >
-- > moreKeybindings = []
-- >
-- > myCursors = makeCursors $ map (map (\x -> [x])) [ "1234", "abc", "xyz"]
-- > -- myCursors = makeCursors [["wsA","wsB","wsC"],["-alpha-","-beta-","-gamma-"],["x","y"]]


-- $todo
--
-- * Find and document how to raise the allowable length of arguments:
--   restoring xmonad's state results in: @xmonad: executeFile: resource
--   exhausted (Argument list too long)@ when you specify more than about 50
--   workspaces. Or change it such that workspaces are created when you try to
--   view it.
--
-- * Function for pretty printing for DynamicLog that groups workspaces by
-- common prefixes
--
-- * Examples of adding workspaces to the cursors, having them appear multiple
--   times for being able to show jumping to some n'th multiple workspace

-- | makeCursors requires a nonempty string, and each sublist must be nonempty
makeCursors ::  [[String]] -> Cursors String
makeCursors [] = error "Workspace Cursors cannot be empty"
makeCursors a = concat . reverse <$> foldl addDim x xs
    where x = end $ map return $ head a
          xs = map (map return) $ tail a
          -- this could probably be simplified, but this true:
          -- toList . makeCursors == map (concat . reverse) . sequence . reverse . map (map (:[]))
          -- the strange order is used because it makes the regular M-1..9
          -- bindings change the prefixes first

addDim ::  (Monoid a) => Cursors a -> [a] -> Cursors a
addDim prev prefixes = Cons . fromJust . W.differentiate
            $ map ((<$> prev) . mappend) prefixes

end :: [a] -> Cursors a
end = Cons . fromJust . W.differentiate . map End

data Cursors a
    = Cons (W.Stack (Cursors a))
    | End a deriving (Eq,Show,Read,Typeable)

instance Foldable Cursors where
    foldMap f (End x) = f x
    foldMap f (Cons (W.Stack x y z)) = foldMap f x `mappend` mconcat (map (foldMap f) $ reverse y ++ z)

instance Functor Cursors where
    fmap f (End a) = End $ f a
    fmap f (Cons (W.Stack x y z)) = Cons $ W.Stack (fmap f x) (fmap (fmap f) y) (fmap (fmap f) z)

changeFocus ::  (Cursors t -> Bool) -> Cursors t -> [Cursors t]
changeFocus p (Cons x) = do
    choose <- chFocus p x
    foc    <- changeFocus p $ W.focus choose
    return . Cons $ choose { W.focus = foc }
changeFocus p x = guard (p x) >> return x

chFocus :: (a -> Bool) -> W.Stack a -> [W.Stack a]
chFocus p st = filter (p . W.focus) $ zipWith const (iterate W.focusDown' st) (W.integrate st)

getFocus ::  Cursors b -> b
getFocus (Cons x) = getFocus $ W.focus x
getFocus (End x) = x

-- This could be made more efficient, if the fact that the suffixes are grouped
focusTo ::  (Eq t) => t -> Cursors t -> Maybe (Cursors t)
focusTo x = listToMaybe . filter ((x==) . getFocus) . changeFocus (const True)

-- | non-wrapping version of 'W.focusUp''
noWrapUp ::  W.Stack t -> W.Stack t
noWrapUp (W.Stack t (l:ls) rs) = W.Stack l ls (t:rs)
noWrapUp x@(W.Stack _ []   _ ) = x

-- | non-wrapping version of 'W.focusDown''
noWrapDown ::  W.Stack t -> W.Stack t
noWrapDown = reverseStack . noWrapUp . reverseStack
    where reverseStack (W.Stack t ls rs) = W.Stack t rs ls

focusDepth ::  Cursors t -> Int
focusDepth (Cons x) = 1 + focusDepth (W.focus x)
focusDepth (End  _) = 0

descend :: Monad m =>(W.Stack (Cursors a) -> m (W.Stack (Cursors a)))-> Int-> Cursors a-> m (Cursors a)
descend f 1 (Cons x) = Cons <$> f x
descend f n (Cons x) | n > 1 = fmap Cons $ descend f (pred n) `onFocus` x
descend _ _ x = return x

onFocus :: (Monad m) => (a1 -> m a1) -> W.Stack a1 -> m (W.Stack a1)
onFocus f st = (\x -> st { W.focus = x}) <$> f (W.focus st)

-- | @modifyLayer@ is used to change the focus at a given depth
modifyLayer :: (W.Stack (Cursors String) -> W.Stack (Cursors String)) -> Int -> X ()
modifyLayer f depth = modifyCursors (descend (return . f) depth)

-- | @shiftModifyLayer@ is the same as 'modifyLayer', but also shifts the
-- currently focused window to the new workspace
shiftModifyLayer :: (W.Stack (Cursors String) -> W.Stack (Cursors WorkspaceId))-> Int-> X ()
shiftModifyLayer f = modifyLayer' $ \st -> do
    let st' = f st
    windows $ W.shift $ getFocus (Cons st')
    return st'

-- | @shiftLayer@ is the same as 'shiftModifyLayer', but the focus remains on
-- the current workspace.
shiftLayer :: (W.Stack (Cursors String) -> W.Stack (Cursors WorkspaceId))-> Int-> X ()
shiftLayer f = modifyLayer' $ \st -> do
    windows $ W.shift $ getFocus $ Cons $ f st
    return st

-- | example usages are 'shiftLayer' and 'shiftModifyLayer'
modifyLayer' :: (W.Stack (Cursors String) -> X (W.Stack (Cursors String))) -> Int -> X ()
modifyLayer' f depth = modifyCursors (descend f depth)

modifyCursors ::  (Cursors String -> X (Cursors String)) -> X ()
modifyCursors = sendMessage . ChangeCursors . (liftA2 (>>) updateXMD return <=<)

data WorkspaceCursors a = WorkspaceCursors (Cursors String)
    deriving (Typeable,Read,Show)

-- | The state is stored in the 'WorkspaceCursors' layout modifier. Put this as
-- your outermost modifier, unless you want different cursors at different
-- times (using "XMonad.Layout.MultiToggle")
workspaceCursors :: Cursors String -> l a -> ModifiedLayout WorkspaceCursors l a
workspaceCursors = ModifiedLayout . WorkspaceCursors

data ChangeCursors = ChangeCursors { unWrap :: Cursors String -> X (Cursors String) }
    deriving (Typeable)

instance Message ChangeCursors

updateXMD ::  Cursors WorkspaceId -> X ()
updateXMD cs = do
    changed <- gets $ (getFocus cs /=) . W.currentTag . windowset
    when changed $ windows $ W.greedyView $ getFocus cs

instance LayoutModifier WorkspaceCursors a where
    redoLayout (WorkspaceCursors cs) _ _ arrs = do
        cws <- gets $ W.currentTag . windowset
        return (arrs,WorkspaceCursors <$> focusTo cws cs)

    handleMess (WorkspaceCursors cs) m =
        sequenceA $ fmap WorkspaceCursors . ($ cs) . unWrap <$> fromMessage m
