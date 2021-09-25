-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Actions.SwapPromote
-- Description :  Track the master window history per workspace.
-- Copyright   :  (c) 2018 Yclept Nemo
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :
-- Stability   :  unstable
-- Portability :  unportable
--
-- Module for tracking master window history per workspace, and associated
-- functions for manipulating the stack using such history.
--
-----------------------------------------------------------------------------


module XMonad.Actions.SwapPromote
    ( -- * Usage
      -- $usage
      MasterHistory (..)
      -- * State Accessors
    , getMasterHistoryMap
    , getMasterHistoryFromTag
    , getMasterHistoryCurrent
    , getMasterHistoryFromWindow
    , modifyMasterHistoryFromTag
    , modifyMasterHistoryCurrent
      -- * Log Hook
    , masterHistoryHook
      -- * Log Hook Building Blocks
    , masterHistoryHook'
    , updateMasterHistory
      -- * Actions
    , swapPromote
    , swapPromote'
    , swapIn
    , swapIn'
    , swapHybrid
    , swapHybrid'
      -- * Action Building Blocks
    , swapApply
    , swapPromoteStack
    , swapInStack
    , swapHybridStack
      -- * List Utilities
    , cycleN
    , split
    , split'
    , merge
    , merge'
      -- * Stack Utilities
    , stackSplit
    , stackMerge
    ) where


import           XMonad
import           XMonad.Prelude
import qualified XMonad.StackSet                as W
import qualified XMonad.Util.ExtensibleState    as XS

import qualified Data.Map                       as M
import qualified Data.Set                       as S
import           Control.Arrow


-- $usage
-- Given your configuration file, import this module:
--
-- > import XMonad.Actions.SwapPromote
--
-- First add 'masterHistoryHook' to your 'logHook' to track master windows per
-- workspace:
--
-- > myLogHook = otherHook >> masterHistoryHook
--
-- Then replace xmonad's default promote keybinding with 'swapPromote'':
--
-- > , ((mod1Mask, xK_Return), swapPromote' False)
--
-- Depending on your xmonad configuration or window actions the master history
-- may be empty. If this is the case you can still chain another promotion
-- function:
--
-- > import XMonad.Actions.DwmPromote
-- > , ((mod1Mask, xK_Return), whenX (swapPromote False) dwmpromote)
--
-- To be clear, this is only called when the lack of master history hindered
-- the swap and not other conditions, such as having a only a single window.
--
-- While 'swapPromote' preserves window focus, 'swapIn' preserves the focus
-- position - effectively "swapping" new windows into focus without moving the
-- zipper. A mix of both, 'swapHybrid' promotes focused non-master windows
-- while swapping windows into the focused master. This works well on layouts
-- with large masters. Both come with chainable variants, see 'swapIn'' and
-- 'swapHybrid''.
--
-- So far floating windows have been treated no differently than tiled windows
-- even though their positions are independent of the stack. Often, yanking
-- floating windows in and out of the workspace will obliterate the stack
-- history - particularly frustrating with 'XMonad.Util.Scratchpad' since it is
-- toggled so frequenty and always replaces the master window. That's why the
-- swap functions accept a boolean argument; when @True@ non-focused floating
-- windows will be ignored.
--
-- All together:
--
-- > , ((mod1Mask, xK_Return), whenX (swapHybrid True) dwmpromote)


-- | Mapping from workspace tag to master history list. The current master is
-- the head of the list, the previous master the second element, and so on.
-- Without history, the list is empty.
newtype MasterHistory = MasterHistory
    { getMasterHistory :: M.Map WorkspaceId [Window]
    } deriving (Read,Show)

instance ExtensionClass MasterHistory where
    initialValue = MasterHistory M.empty

-- | Return the master history map from the state.
getMasterHistoryMap :: X (M.Map WorkspaceId [Window])
getMasterHistoryMap = XS.gets getMasterHistory

-- | Return the master history list of a given tag. The master history list may
-- be empty. An invalid tag will also result in an empty list.
getMasterHistoryFromTag :: WorkspaceId -> X [Window]
getMasterHistoryFromTag t = M.findWithDefault [] t <$> getMasterHistoryMap

-- | Return the master history list of the current workspace.
getMasterHistoryCurrent :: X [Window]
getMasterHistoryCurrent =   gets (W.currentTag . windowset)
                        >>= getMasterHistoryFromTag

-- | Return the master history list of the workspace containing the given
-- window. Return an empty list if the window is not in the stackset.
getMasterHistoryFromWindow :: Window -> X [Window]
getMasterHistoryFromWindow w =   gets (W.findTag w . windowset)
                             >>= maybe (return []) getMasterHistoryFromTag

-- | Modify the master history list of a given workspace, or the empty list of
-- no such workspace is mapped. The result is then re-inserted into the master
-- history map.
modifyMasterHistoryFromTag :: WorkspaceId -> ([Window] -> [Window]) -> X ()
modifyMasterHistoryFromTag t f = XS.modify $ \(MasterHistory m) ->
    let l = M.findWithDefault [] t m
    in  MasterHistory $ M.insert t (f l) m

-- | Modify the master history list of the current workspace. While the current
-- workspace is guaranteed to exist; its master history may not. For more
-- information see 'modifyMasterHistoryFromTag'.
modifyMasterHistoryCurrent :: ([Window] -> [Window]) -> X ()
modifyMasterHistoryCurrent f =   gets (W.currentTag . windowset)
                             >>= flip modifyMasterHistoryFromTag f

-- | A 'logHook' to update the master history mapping. Non-existent workspaces
-- are removed, and the master history list for the current workspaces is
-- updated. See 'masterHistoryHook''.
masterHistoryHook :: X ()
masterHistoryHook = masterHistoryHook' True updateMasterHistory

-- | Backend for 'masterHistoryHook'.
masterHistoryHook' :: Bool
                        -- ^ If @True@, remove non-existent workspaces.
                   -> ([Window] -> [Window] -> [Window])
                        -- ^ Function used to update the master history list of
                        -- the current workspace. First argument is the master
                        -- history, second is the integrated stack.  See
                        -- 'updateMasterHistory' for more details.
                   -> X ()
masterHistoryHook' removeWorkspaces historyModifier = do
    wset <- gets windowset
    let W.Workspace wid _ mst = W.workspace . W.current $ wset
        tags = map W.tag $ W.workspaces wset
        st = W.integrate' mst
    XS.modify $ \(MasterHistory mm) ->
        let mm' = if removeWorkspaces
                  then restrictKeys mm $ S.fromList tags
                  else mm
            ms  = M.findWithDefault [] wid mm'
            ms' = historyModifier ms st
        in  MasterHistory $ M.insert wid ms' mm'

-- | Less efficient version of 'M.restrictKeys'. Given broader eventual
-- adoption, replace this with 'M.restrictKeys'.
restrictKeys :: Ord k => M.Map k a -> S.Set k -> M.Map k a
restrictKeys m s = M.filterWithKey (\k _ -> k `S.member` s) m

-- | Given the current master history list and an integrated stack, return the
-- new master history list. The current master is either moved (if it exists
-- within the history) or added to the head of the list, and all missing (i.e.
-- closed) windows are removed.
updateMasterHistory :: [Window] -- ^ The master history list.
                    -> [Window] -- ^ The integrated stack.
                    -> [Window]
updateMasterHistory _  []       = []
updateMasterHistory ms ws@(w:_) = (w : delete w ms) `intersect` ws

-- | Wrap 'swapPromoteStack'; see also 'swapApply'.
swapPromote :: Bool -> X Bool
swapPromote = flip swapApply swapPromoteStack

-- | Like 'swapPromote'' but discard the result.
swapPromote' :: Bool -> X ()
swapPromote' = void . swapPromote

-- | Wrap 'swapInStack'; see also 'swapApply'.
swapIn :: Bool -> X Bool
swapIn = flip swapApply swapInStack

-- | Like 'swapIn'' but discard the result.
swapIn' :: Bool -> X ()
swapIn' = void . swapIn

-- | Wrap 'swapHybridStack'; see also 'swapApply'.
swapHybrid :: Bool -> X Bool
swapHybrid = flip swapApply swapHybridStack

-- | Like 'swapHybrid'' but discard the result.
swapHybrid' :: Bool -> X ()
swapHybrid' = void . swapHybrid

-- | Apply the given master history stack modifier to the current stack. If
-- given @True@, all non-focused floating windows will be ignored. Return
-- @True@ if insufficient history; if so use 'whenX' to sequence a backup
-- promotion function.
swapApply :: Bool
          -> (Maybe Window -> W.Stack Window -> (Bool,W.Stack Window))
          -> X Bool
swapApply ignoreFloats swapFunction = do
    fl <- gets $ W.floating . windowset
    st <- gets $ W.stack . W.workspace . W.current . windowset
    ch <- getMasterHistoryCurrent
    let swapApply' s1 =
            let fl' = if ignoreFloats then M.keysSet fl else S.empty
                ff = (||) <$> (`S.notMember` fl') <*> (== W.focus s1)
                fh = filter ff ch
                pm = listToMaybe . drop 1 $ fh
                (r,s2) = stackSplit s1 fl' :: ([(Int,Window)],W.Stack Window)
                (b,s3) = swapFunction pm s2
                s4 = stackMerge s3 r
                mh = let w = head . W.integrate $ s3
                     in  const $ w : delete w ch
            in (b,Just s4,mh)
        (x,y,z) = maybe (False,Nothing,id) swapApply' st
    -- Any floating master windows will be added to the history when 'windows'
    -- calls the log hook.
    modifyMasterHistoryCurrent z
    windows $ W.modify Nothing . const $ y
    return x

-- | If the focused window is the master window and there is no previous
-- master, do nothing. Otherwise swap the master with the previous master. If
-- the focused window is not the master window, swap it with the master window.
-- In either case focus follows the original window, i.e. the focused window
-- does not change, only its position.
--
-- The first argument is the previous master (which may not exist), the second
-- a window stack. Return @True@ if the master history hindered the swap; the
-- history is either empty or out-of-sync. Though the latter shouldn't happen
-- this function never changes the stack under such circumstances.
swapPromoteStack :: Maybe Window -> W.Stack Window -> (Bool,W.Stack Window)
swapPromoteStack _         st@(W.Stack _x [] []) = (False,st)
swapPromoteStack Nothing   st@(W.Stack _x [] _r) = (True,st)
swapPromoteStack (Just pm)    (W.Stack  x []  r) =
    let (r',l') = (reverse *** cycleN 1) $ span (/= pm) $ reverse r
        st'     = W.Stack x l' r'
        b       = null l'
    in  (b,st')
swapPromoteStack _            (W.Stack  x l   r) =
    let r'  = (++ r) . cycleN 1 . reverse $ l
        st' = W.Stack x [] r'
    in  (False,st')

-- | Perform the same swap as 'swapPromoteStack'. However the new window
-- receives the focus; it appears to "swap into" the position of the original
-- window. Under this model focus follows stack position and the zipper does
-- not move.
--
-- See 'swapPromoteStack' for more details regarding the parameters.
swapInStack :: Maybe Window -> W.Stack Window -> (Bool,W.Stack Window)
swapInStack _         st@(W.Stack _x [] []) = (False,st)
swapInStack Nothing   st@(W.Stack _x [] _r) = (True,st)
swapInStack (Just pm)    (W.Stack  x []  r) =
    let (x',r') = case span (/= pm) r of
            (__,[]) -> (x,r)
            (sl,sr) -> (pm,sl ++ x : drop 1 sr)
        st'     = W.Stack x' [] r'
        b       = x' == x
    in  (b,st')
swapInStack _            (W.Stack  x l   r) =
    let l'  = init l ++ [x]
        x'  = last l
        st' = W.Stack x' l' r
    in  (False,st')

-- | If the focused window is the master window, use 'swapInStack'. Otherwise use
-- 'swapPromoteStack'.
--
-- See 'swapPromoteStack' for more details regarding the parameters.
swapHybridStack :: Maybe Window -> W.Stack Window -> (Bool,W.Stack Window)
swapHybridStack m st@(W.Stack _ [] _) = swapInStack m st
swapHybridStack m st                  = swapPromoteStack m st

-- | Cycle a list by the given count. If positive, cycle to the left. If
-- negative, cycle to the right:
--
-- >>> cycleN 2 [1,2,3,4,5]
-- [3,4,5,1,2]
-- >>> cycleN (-2) [1,2,3,4,5]
-- [4,5,1,2,3]
cycleN :: Int -> [a] -> [a]
cycleN n ls =
    let l = length ls
    in  take l $ drop (n `mod` l) $ cycle ls

-- | Wrap 'split'' with an initial index of @0@, discarding the list's length.
split :: (Num a, Enum a) => (b -> Bool) -> [b] -> ([(a,b)],[b])
split p l =
    let (_,ys,ns) = split' p 0 l
    in  (ys,ns)

-- | Given a predicate, an initial index and a list, return a tuple containing:
--
--  * List length.
--  * Indexed list of elements which satisfy the predicate. An indexed element
--    is a tuple containing the element index (offset by the initial index) and
--    the element.
--  * List of elements which do not satisfy the predicate.
--
-- The initial index and length of the list simplify chaining calls to this
-- function, such as for zippers of lists.
split' :: (Num a, Enum a) => (b -> Bool) -> a -> [b] -> (a,[(a,b)],[b])
split' p i l =
    let accumulate e (c,ys,ns) = if p (snd e)
            then (c+1,e:ys,ns)
            else (c+1,ys,e:ns)
        (c',ys',ns') = foldr accumulate (0,[],[]) $ zip [i..] l
    in  (c',ys',map snd ns')

-- | Wrap 'merge'' with an initial virtual index of @0@. Return only the
-- unindexed list with elements from the leftover indexed list appended.
merge :: (Ord a, Num a) => [(a,b)] -> [b] -> [b]
merge il ul =
    let (_,il',ul') = merge' 0 il ul
    in  ul' ++ map snd il'

-- | Inverse of 'split'. Merge an indexed list with an unindexed list (see
-- 'split''). Given a virtual index, an indexed list and an unindexed list,
-- return a tuple containing:
--
--  * Virtual index /after/ the unindexed list
--  * Remainder of the indexed list
--  * Merged unindexed list
--
-- If the indexed list is empty, this functions consumes the entire unindexed
-- list. If the unindexed list is empty, this function consumes only adjacent
-- indexed elements. For example, @[(10,"ten"),(12,"twelve")]@ implies missing
-- unindexed elements and so once @(10,"ten")@ is consumed this function
-- concludes.
--
-- The indexed list is assumed to have been created by 'split'' and not checked
-- for correctness. Indices are assumed to be ascending, i.e.
-- > [(1,"one"),(2,"two"),(4,"four")]
--
-- The initial and final virtual indices simplify chaining calls to the this
-- function, as as for zippers of lists. Positive values shift the unindexed
-- list towards the tail, as if preceded by that many elements.
merge' :: (Ord a, Num a) => a -> [(a,b)] -> [b] -> (a,[(a,b)],[b])
merge' i il@((j,a):ps) ul@(b:bs) = if j <= i
    then let (x,y,z) = merge' (i+1) ps ul
         in  (x,y,a:z)
    else let (x,y,z) = merge' (i+1) il bs
         in  (x,y,b:z)
merge' i [] (b:bs) =
         let (x,y,z) = merge' (i+1) [] bs
         in  (x,y,b:z)
merge' i il@((j,a):ps) [] = if j <= i
    then let (x,y,z) = merge' (i+1) ps []
         in  (x,y,a:z)
    else (i,il,[])
merge' i [] [] =
         (i,[],[])

-- | Remove all elements of the set from the stack. Skip the currently focused
-- member. Return an indexed list of excluded elements and the modified stack.
-- Use 'stackMerge' to re-insert the elements using this list.
stackSplit :: (Num a, Enum a, Ord b) => W.Stack b -> S.Set b -> ([(a,b)],W.Stack b)
stackSplit (W.Stack x l r) s =
    let (c,fl,tl) = split' (`S.member` s) 0 (reverse l)
        (_,fr,tr) = split' (`S.member` s) (c+1) r
    in  (fl++fr,W.Stack x (reverse tl) tr)

-- | Inverse of 'stackSplit'. Given a list of elements and their original
-- indices, re-insert the elements into these same positions within the stack.
-- Skip the currently focused member. Works best if the stack's length hasn't
-- changed, though if shorter any leftover elements will be tacked on.
stackMerge :: (Ord a, Num a) => W.Stack b -> [(a,b)] -> W.Stack b
stackMerge (W.Stack x l r) il =
    let (i,il1,l') = merge' 0 il (reverse l)
        (_,il2,r') = merge' (i+1) il1 r
    in  W.Stack x (reverse l') (r' ++ map snd il2)
