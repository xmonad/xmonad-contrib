-------------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Hooks.ClickableWorkspaces
-- Copyright   :  (c) Geoff deRosenroll <geoffderosenroll@gmail.com>
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  Geoff deRosenroll <geoffderosenroll@gmail.com>
-- Stability   :  unstable
-- Portability :  unportable
--
-----------------------------------------------------------------------------

module ClickableWsHook ( doClickableWsHook
                       , undoClickableWsHook
                       , getClickableSortByWsTag
                       , getSortByClickableIndex
                       , getClickableSortByXineramaRuleAndTag
                       , getClickableSortByXineramaRuleAndIndex
                       , getClickableSortByXineramaPhysicalRuleAndTag
                       , getClickableSortByXineramaPhysicalRuleAndIndex
                       ) where

import XMonad
import qualified XMonad.StackSet as W

import XMonad.Actions.PhysicalScreens ( ScreenComparator(ScreenComparator)
                                      , getScreenIdAndRectangle
                                      , screenComparatorById )
import XMonad.Util.WorkspaceCompare

import Data.Function (on)
import Data.List
import Data.Maybe (fromJust)
import qualified Data.Map as M

-----------------------------------------------------------------------------
-- | Wrapping workspace tags with on-click xdotool actions (requires
--   xdotool in path). Also remember to replace StdinReader with
--   UnsafeStdinReader in your XMobar config to allow for these action tags.

-- In case workspace tags include any '<', escape them
xmobarEscape :: String -> String
xmobarEscape = concatMap doubleLts
  where
    doubleLts '<' = "<<"
    doubleLts x   = [x]

clickableWrap :: (Integer, String) -> String
clickableWrap (num, ws) =
  "<action=xdotool key super+" ++ show num ++ ">"
  ++ xmobarEscape ws ++ "</action>"

-----------------------------------------------------------------------------
-- | Non Xinerama functions, similar to Xmonad.Util.WorkspaceCompare, but
--   swapping out / accounting for clickable wrapped workspaces as needed.

-- Get workspace tags from user config (e.g. myWorkspaces)
configWorkspaces :: X [WorkspaceId]
configWorkspaces = asks (workspaces . config)

-- Wrap workspace tags with on-click switch action
clickableWorkspaces :: X [WorkspaceId]
clickableWorkspaces =
  configWorkspaces >>= \ws ->
    return $ map clickableWrap $ zip [1 :: Integer .. 9] ws

-- Compare Maybe's differently, so Nothing (i.e. workspaces without indexes)
-- come last in the order. From XMonad.Util.WorkspaceCompare (not exported)
indexCompare :: Maybe Int -> Maybe Int -> Ordering
indexCompare Nothing Nothing = EQ
indexCompare Nothing (Just _) = GT
indexCompare (Just _) Nothing = LT
indexCompare a b = compare a b

getClickableIndex :: X (WorkspaceId -> Maybe Int)
getClickableIndex =
  clickableWorkspaces >>= \ws -> return $ flip elemIndex ws

getClickableCompare :: X WorkspaceCompare
getClickableCompare =
  getClickableIndex >>= \idx ->
    return $ mconcat [indexCompare `on` idx, compare]

-- Sort workspaces by index of clickableWorkpaces (for ppSort)
getSortByClickableIndex :: X WorkspaceSort
getSortByClickableIndex = mkWsSort getClickableCompare

getWsTagFromClickable :: X (WorkspaceId -> Maybe String)
getWsTagFromClickable = do
  spaces <- configWorkspaces
  clicks <- clickableWorkspaces
  return $ flip M.lookup $ M.fromList $ zip clicks spaces

getClickableCompareByWsTag :: X WorkspaceCompare
getClickableCompareByWsTag =
  getWsTagFromClickable >>= \tag -> return (compare `on` tag)

-- Sort workspaces by original names from config (for ppSort)
getClickableSortByWsTag :: X WorkspaceSort
getClickableSortByWsTag = mkWsSort getClickableCompareByWsTag

-----------------------------------------------------------------------------
-- | Xinerama sorting functions adapted to take clickable tags into account
--   (see XMonad.Util.WorkspaceCompare for originals/comments)

-- Simple clickable comparisons for Xinerama compare functions
clickableIndexOrdering :: X (WorkspaceId -> WorkspaceId -> Ordering)
clickableIndexOrdering =
  getClickableIndex >>= \idx -> return $ \a b -> indexCompare (idx a) (idx b)

clickableTagOrdering :: X (WorkspaceId -> WorkspaceId -> Ordering)
clickableTagOrdering =
  getWsTagFromClickable >>= \tag -> return $ \a b -> compare (tag a) (tag b)

getXineramaPhysicalClickableWsCompare ::
  ScreenComparator
  -> X (WorkspaceId -> WorkspaceId -> Ordering)
  -> X WorkspaceCompare
getXineramaPhysicalClickableWsCompare (ScreenComparator sc) wsOrderer = do
    w <- gets windowset
    offscreenOrderer <- wsOrderer
    return $ \ a b -> case (isOnScreen a w, isOnScreen b w) of
        (True, True)   -> compareUsingScreen w a b
        (False, False) -> offscreenOrderer a b
        (True, False)  -> LT
        (False, True)  -> GT
  where
    onScreen w =  W.current w : W.visible w
    isOnScreen a w  = a `elem` map (W.tag . W.workspace) (onScreen w)
    tagToScreen s x = fromJust $ find ((== x) . W.tag . W.workspace) s
    compareUsingScreen w = sc `on` getScreenIdAndRectangle . tagToScreen (onScreen w)

getXineramaClickableWsCompare ::
  X (WorkspaceId -> WorkspaceId -> Ordering)
  -> X WorkspaceCompare
getXineramaClickableWsCompare wsOrderer =
  getXineramaPhysicalClickableWsCompare (screenComparatorById compare) wsOrderer

getClickableSortByXineramaRuleAndTag :: X WorkspaceSort
getClickableSortByXineramaRuleAndTag =
  mkWsSort $ getXineramaClickableWsCompare clickableTagOrdering

getClickableSortByXineramaRuleAndIndex :: X WorkspaceSort
getClickableSortByXineramaRuleAndIndex =
  mkWsSort $ getXineramaClickableWsCompare clickableIndexOrdering

getClickableSortByXineramaPhysicalRuleAndTag:: ScreenComparator -> X WorkspaceSort
getClickableSortByXineramaPhysicalRuleAndTag sc =
  mkWsSort $ getXineramaPhysicalClickableWsCompare sc clickableTagOrdering

getClickableSortByXineramaPhysicalRuleAndIndex :: ScreenComparator -> X WorkspaceSort
getClickableSortByXineramaPhysicalRuleAndIndex sc =
  mkWsSort $ getXineramaPhysicalClickableWsCompare sc clickableIndexOrdering

-----------------------------------------------------------------------------
-- | Place these hooks around dynamicLogWithPP in your LogHook, so workspace
--   names are wrapped with clickable action tags only for the moment that they
--   are piped into XMobar.
--
--   myLogHook xmproc = ... <+> doClickableHook <+> dynamicLogWithPP xmobarPP
--     { ...
--     , ppSort = getSortByClickableIndex
--     } <+> undoClickableHook

doClickableWsHook :: X ()
doClickableWsHook = do
  spaces <- configWorkspaces
  clicks <- clickableWorkspaces
  _ <- mapM (\(w, c) -> modifyWindowSet $ \s -> W.renameTag w c s)
       $ zip spaces clicks
  return ()

undoClickableWsHook :: X ()
undoClickableWsHook = do
  spaces <- configWorkspaces
  clicks <- clickableWorkspaces
  _ <- mapM (\(w, c) -> modifyWindowSet $ \s -> W.renameTag c w s)
       $ zip spaces clicks
  return ()
