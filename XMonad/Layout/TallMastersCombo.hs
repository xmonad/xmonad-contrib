-- {-# LANGUAGE PatternGuards, FlexibleContexts, FlexibleInstances, DeriveDataTypeable, TypeSynonymInstances, MultiParamTypeClasses #-}
{-# LANGUAGE PatternGuards,
    FlexibleContexts, 
    FlexibleInstances, 
    DeriveDataTypeable, 
    TypeSynonymInstances, 
    MultiParamTypeClasses, 
    UndecidableInstances
#-}
---------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.TallMastersCombo
-- Copyright   :  (c) 2019 Ningji Wei
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  Ningji Wei <tidues@gmail.com>
-- Stability   :  unstable
-- Portability :  unportable
--
-- A layout combinator that support Shrink, Expand, and IncMasterN just as the 
-- 'Tall' layout, and also support operations of two master windows: 
-- a main master, which is the original master window;
-- a sub master, the first window of the second pane.
-- This combinator can be nested, and has a good support for using 
-- 'XMonad.Layout.Tabbed' as a sublayout.
--
-----------------------------------------------------------------------------

module XMonad.Layout.TallMastersCombo (
  -- * Usage
  -- $usage
  tmsCombineTwoDefault,
  tmsCombineTwo,
  TMSCombineTwo (..),
  RowsOrColumns (..),
  Choose1 (..),

  -- * Messages
  SwitchOrientation (..),
  SwapSubMaster (..),
  FocusSubMaster (..),
  FocusedNextLayout (..),
  TestMsg (..),

  -- * Utilities
  (|||),
  swapWindow,
  focusWindow,
  handleMessages
) where

import XMonad hiding (focus, (|||))
import XMonad.StackSet (Workspace(..),integrate',Stack(..))
import qualified XMonad.StackSet as W
import Data.Maybe (fromJust,isJust,fromMaybe)
import Data.List (delete)
import Control.Monad (join, foldM)
import XMonad.Layout (Choose, ChangeLayout(..))
import qualified XMonad.Layout as LL
import Data.Typeable
import XMonad.Layout.Simplest
import XMonad.Layout hiding ((|||))
import XMonad.Layout.Tabbed (tabbed, fontName, shrinkText)


---------------------------------------------------------------------------------
-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Layout.TallMastersCombo
--
-- and add something like
--
-- > tmsCombineTwoDefault (Tall 0 (3/100) 0) simpleTabbed
--
-- This will make the 'Tall' as the master pane, and 'simpleTabbed' as the second pane. 
-- You can shrink, expand, and increase more windows to the master pane just like using the
-- 'Tall' layout.
--
-- To swap and/or focus the sub master window (the first window in the second pane), you can add
-- the following key bindings
--
-- >      , ((modm .|. shiftMask, m),         sendMessage $ FocusSubMaster)
-- >      , ((modm .|. shiftMask, xK_Return), sendMessage $ SwapSubMaster)
--
-- In each pane, you can use multiple layouts with 'Choose' combinator, and switch
-- between them with the 'FocusedNextLayout' message. Below is one example
--
-- > layout1 = Simplest ||| Tabbed
-- > layout2 = Full ||| Tabbed ||| (RowsOrColumns True)
-- > myLayout = tmsCombineTwoDefault layout1 layout2
--
-- then add the following key binding,
--
-- >      , ((modm, w), sendMessage $ FocusedNextLayout)
-- 
-- Now, pressing this key will toggle the multiple layouts in the currently focused pane.
--
-- You can mirror this layout with the default 'Mirror' key binding. But to have a more natural
-- behaviors, you can use the 'SwitchOrientation' message:
--
-- >      , ((modm, xK_space), sendMessage $ SwitchOrientation)
-- 
-- This will not mirror the tabbed decoration, and will keep sublayouts that made by TallMastersCombo
-- and RowsOrColumns display in natural orientations.
--
-- To merge layouts more flexibly, you can use 'tmsCombineTwo' instead.
--
-- > tmsCombineTwo False 1 (3/100) (1/3) Simplest simpleTabbed
--
-- This creates a vertical merged layout with 1 window in the master pane, and the master pane shrinks 
-- and expands with a step of (3\/100), and occupies (1\/3) of the screen.
--
-- Finally, this combinator can be nested. Here is one example,
--
-- @
-- subLayout  = tmsCombineTwo False 1 (3\/100) (1\/2) Simplest simpleTabbed
-- layout1    = simpleTabbed ||| subLayout
-- layout2    = subLayout ||| simpleTabbed ||| (RowsOrColumns True)
-- baseLayout = tmsCombineTwoDefault layout1 layout2
--
-- mylayouts = smartBorders $
--             avoidStruts $
--             mkToggle (FULL ?? EOT) $
--             baseLayout
-- @
--
-- this is a realization of the cool idea from
--
-- <https://www.reddit.com/r/xmonad/comments/3vkrc3/does_this_layout_exist_if_not_can_anyone_suggest/>
--
-- and is more flexible.
--

-- | A simple layout that arranges windows in a row or a column with equal sizes.
-- It can switch between row mode and column mode by listening to the message 'SwitchOrientation'.
data RowsOrColumns a = RowsOrColumns { rowMode :: Bool -- ^ arrange windows in rows or columns
                                     } deriving (Show, Read)

instance LayoutClass RowsOrColumns Window where
  description (RowsOrColumns rows) = 
    if rows then "Rows" else "Columns"

  pureLayout (RowsOrColumns rows) r s = zip ws rs
    where ws = W.integrate s
          len = length ws
          rs = if rows
               then splitVertically len r
               else splitHorizontally len r
    
  pureMessage (RowsOrColumns rows) m
    | Just Row <- fromMessage m = Just $ RowsOrColumns True
    | Just Col <- fromMessage m = Just $ RowsOrColumns False
    | otherwise = Nothing


data TMSCombineTwo l1 l2 a = 
  TMSCombineTwo { focusLst :: [a]
                , ws1 :: [a]
                , ws2 :: [a]
                , columnMode :: Bool  -- ^ merge two layouts in a column or a row
                , nMaster :: !Int     -- ^ number of windows in the master pane
                , rationInc :: !Rational -- ^ percent of screen to increment by when resizing panes
                , tallComboRatio :: !Rational -- ^ default proportion of screen occupied by master pane
                , layoutFst :: l1 a  -- ^ layout for the master pane
                , layoutSnd :: l2 a  -- ^ layout for the second pane
                }
        deriving (Show, Read)

-- | Combine two layouts l1 l2 with default behaviors.
tmsCombineTwoDefault :: (LayoutClass l1 Window, LayoutClass l2 Window) => 
                          l1 Window -> l2 Window -> TMSCombineTwo l1 l2 Window
tmsCombineTwoDefault = TMSCombineTwo [] [] [] True 1 (3/100) (1/2)

-- | A more flexible way of merging two layouts. User can specify if merge them vertical or horizontal,
-- the number of windows in the first pane (master pane), the shink and expand increment, and the proportion
-- occupied by the master pane.
tmsCombineTwo :: (LayoutClass l1 Window, LayoutClass l2 Window) => 
                  Bool -> Int -> Rational -> Rational -> l1 Window -> l2 Window -> TMSCombineTwo l1 l2 Window
tmsCombineTwo = TMSCombineTwo [] [] []

data Orientation = Row | Col deriving (Read, Show, Typeable)
instance Message Orientation

-- | A message that switches the orientation of TallMasterCombo layout and the RowsOrColumns layout.
-- This is similar to the 'Mirror' message, but 'Mirror' cannot apply to hidden layouts, and when 'Mirror' 
-- applies to the 'XMonad.Layout.Tabbed' decoration, it will also mirror the tabs, which may lead to unintended 
-- visualizations. The 'SwitchOrientation' message refreshes layouts according to the orientation of the parent layout, 
-- and will not affect the 'XMonad.Layout.Tabbed' decoration.
data SwitchOrientation = SwitchOrientation deriving (Read, Show, Typeable)
instance Message SwitchOrientation

-- | This message swaps the current focused window with the sub master window (first window in the second pane).
data SwapSubMaster = SwapSubMaster deriving (Read, Show, Typeable)
instance Message SwapSubMaster

-- | This message changes the focus to the sub master window (first window in the second pane).
data FocusSubMaster = FocusSubMaster deriving (Read, Show, Typeable)
instance Message FocusSubMaster

-- | This message triggers the 'NextLayout' message in the pane that contains the focused window.
data FocusedNextLayout = FocusedNextLayout deriving (Read, Show, Typeable)
instance Message FocusedNextLayout

-- | This is a message for testing
data TestMsg = TestMsg deriving (Read, Show, Typeable)
instance Message TestMsg

-- instance (Typeable l1, Typeable l2, LayoutClass l1 Window, LayoutClass l2 Window) => LayoutClass (TMSCombineTwo l1 l2) Window where
instance (Typeable l1, Typeable l2, GetFocused l1 Window, GetFocused l2 Window) => LayoutClass (TMSCombineTwo l1 l2) Window where
  description _ = "TallMasters"

  runLayout (Workspace wid l@(TMSCombineTwo f w1 w2 vsp nmaster delta frac layout1 layout2) s) r = 
      let slst = integrate' s
          f0 = getFocused l s
          f' = case s of (Just s') -> focus s':delete (focus s') f0
                         Nothing   -> f0
          layStr1 = showInfo layout1
          layStr2 = showInfo layout2
          layStr  = layStr1 ++ layStr2
          snum = length(slst)
          (slst1, slst2) = splitAt nmaster slst
          s0 = differentiate f' slst
          s1' = differentiate f' slst1
          s2' = differentiate f' slst2
          (s1,s2,frac') | nmaster == 0    = (Nothing,s0,0)
                        | nmaster >= snum = (s0,Nothing,1)
                        | otherwise       = (s1',s2',frac)
          (r1, r2) = if vsp
                     then splitHorizontallyBy frac' r
                     else splitVerticallyBy frac' r
      in 
      do 
         spawn $ "echo '" ++ layStr ++ "' >> ~/.xmonad/debug2.txt"
         (ws1,ml1) <- runLayout (Workspace wid layout1 s1) r1
         (ws2,ml2) <- runLayout (Workspace wid layout2 s2) r2
         -- let str1 = "echo 'ml1:" ++ show (typeOf ml1) ++ "' >> ~/.xmonad/debug1.txt"
         --     str2 = "echo 'ml2:" ++ show (typeOf ml2) ++ "' >> ~/.xmonad/debug1.txt"
         -- spawn $ str1
         -- spawn $ str2
         let newlayout1 = maybe layout1 id ml1
             newlayout2 = maybe layout2 id ml2
         return (ws1++ws2, Just $ TMSCombineTwo f' slst1 slst2 vsp nmaster delta frac newlayout1 newlayout2)


  handleMessage i@(TMSCombineTwo f w1 w2 vsp nmaster delta frac layout1 layout2) m
    -- messages that only traverse one level
    | Just Shrink <- fromMessage m = return . Just $ TMSCombineTwo f w1 w2 vsp nmaster delta (max 0 $ frac-delta) layout1 layout2
    | Just Expand <- fromMessage m = return . Just $ TMSCombineTwo f w1 w2 vsp nmaster delta (min 1 $ frac+delta) layout1 layout2
    | Just (IncMasterN d) <- fromMessage m = 
        let w = w1++w2
            nmasterNew = min (max 0 (nmaster+d)) (length w)
            (w1',w2')  = splitAt nmasterNew w
        in return . Just $ TMSCombineTwo f w1' w2' vsp nmasterNew delta frac layout1 layout2
    | Just SwitchOrientation <- fromMessage m = 
            let m1 = if vsp then SomeMessage Col else SomeMessage Row
            in
            do mlayout1 <- handleMessage layout1 m1
               mlayout2 <- handleMessage layout2 m1
               return $ mergeSubLayouts  mlayout1 mlayout2 (TMSCombineTwo f w1 w2 (not vsp) nmaster delta frac layout1 layout2) True
    | Just SwapSubMaster <- fromMessage m = 
        -- first get the submaster window
        let subMaster = if null w2 then Nothing else Just $ head w2
        in case subMaster of
            Just mw -> do windows $ W.modify' $ swapWindow mw
                          return Nothing
            Nothing -> return Nothing
    | Just FocusSubMaster <- fromMessage m =
        -- first get the submaster window
        let subMaster = if null w2 then Nothing else Just $ head w2
        in case subMaster of
            Just mw -> do windows $ W.modify' $ focusWindow mw
                          return Nothing
            Nothing -> return Nothing
    -- messages that traverse recursively
    | Just Row <- fromMessage m =
        do mlayout1 <- handleMessage layout1 (SomeMessage Col)
           mlayout2 <- handleMessage layout2 (SomeMessage Col)
           return $ mergeSubLayouts mlayout1 mlayout2 (TMSCombineTwo f w1 w2 False nmaster delta frac layout1 layout2) True
    | Just Col <- fromMessage m =
        do mlayout1 <- handleMessage layout1 (SomeMessage Row)
           mlayout2 <- handleMessage layout2 (SomeMessage Row)
           return $ mergeSubLayouts mlayout1 mlayout2 (TMSCombineTwo f w1 w2 True nmaster delta frac layout1 layout2) True
    | Just FocusedNextLayout <- fromMessage m = 
       do
       -- All toggle message is passed to the sublayout with focused window
         mst <- gets (W.stack . W.workspace . W.current . windowset) 
         let focId = findFocused mst w1 w2
             m1 = if vsp then SomeMessage Row else SomeMessage Col
         if focId == 1
           then do 
                 mlay1 <- handleMessages layout1 [(SomeMessage NextLayout), m1]
                 let mlay2 = Nothing
                 return $ mergeSubLayouts mlay1 mlay2 i True
           else do
                 let mlay1 = Nothing
                 mlay2 <- handleMessages layout2 [(SomeMessage NextLayout), m1]
                 return $ mergeSubLayouts mlay1 mlay2 i True
    | Just TestMsg <- fromMessage m = 
       do
         spawn $ "xmessage " ++ show f
         return Nothing
    | otherwise = 
            do
              mlayout1 <- handleMessage layout1 m
              mlayout2 <- handleMessage layout2 m
              return $ mergeSubLayouts mlayout1 mlayout2 i False



-- code from CombineTwo
-- given two sets of zs and xs takes the first z from zs that also belongs to xs
-- and turns xs into a stack with z being current element. Acts as
-- StackSet.differentiate if zs and xs don't intersect
differentiate :: Eq q => [q] -> [q] -> Maybe (Stack q)
differentiate (z:zs) xs | z `elem` xs = Just $ Stack { focus=z
                                                     , up = reverse $ takeWhile (/=z) xs
                                                     , down = tail $ dropWhile (/=z) xs }
                        | otherwise = differentiate zs xs
differentiate [] xs = W.differentiate xs

-- | Swap a given window with the focused window.
swapWindow :: (Eq a) => a -> Stack a -> Stack a
swapWindow w s = 
  let upLst   = up s
      foc     = focus s
      downLst = down s
  in if elem w (downLst)
     then let us   = takeWhile (/= w) downLst
              d:ds = dropWhile (/= w) downLst
              us'  = reverse us ++ d : upLst
          in  Stack foc us' ds
     else let ds   = takeWhile (/= w) upLst
              u:us = dropWhile (/= w) upLst
              ds'  = reverse ds ++ u : downLst
          in  Stack foc us ds'


-- | Focus a given window.
focusWindow :: (Eq a) => a -> Stack a -> Stack a
focusWindow w s = 
  if elem w (up s) 
  then focusSubMasterU w s
  else focusSubMasterD w s
  where
      focusSubMasterU w i@(Stack foc (l:ls) rs) = 
          if foc == w
          then i
          else
              if l == w
              then news
              else focusSubMasterU w news
              where news = Stack l ls (foc:rs)
      focusSubMasterU w (Stack foc [] rs) = 
          Stack foc [] rs
      focusSubMasterD w i@(Stack foc ls (r:rs)) =
          if foc == w
          then i
          else
              if r == w
              then news
              else focusSubMasterD w news
              where news = Stack r (foc:ls) rs
      focusSubMasterD w (Stack foc ls []) = 
          Stack foc ls []

-- | Merge two Maybe sublayouts.
mergeSubLayouts ml1 ml2 (TMSCombineTwo f w1 w2 vsp nmaster delta frac l1 l2) alwaysReturn =
  if alwaysReturn
  then Just $ TMSCombineTwo f w1 w2 vsp nmaster delta frac (maybe l1 id ml1) (maybe l2 id ml2)
  else
    if isJust ml1 || isJust ml2
    then Just $ TMSCombineTwo f w1 w2 vsp nmaster delta frac (maybe l1 id ml1) (maybe l2 id ml2)
    else Nothing

findFocused :: (Eq a) => Maybe (Stack a) -> [a] -> [a] -> Int
findFocused mst w1 w2 =
        case mst of
          Nothing -> 1
          Just st -> if elem foc w1
                     then 1
                     else if elem foc w2
                          then 2
                          else 1
                     where foc = W.focus st

-- | Handle a list of messages one by one, then return the last refreshed layout.
handleMessages :: (LayoutClass l a) => l a -> [SomeMessage] -> X (Maybe (l a))
handleMessages l ms = foldM  handleMaybeMsg (Just l) ms

handleMaybeMsg :: (LayoutClass l a) => Maybe (l a) -> SomeMessage -> X (Maybe (l a))
handleMaybeMsg ml m = case ml of Just l  -> do 
                                              res <- handleMessage l m
                                              return $ elseOr (Just l) res
                                 Nothing -> return Nothing

-- right biased maybe merge
elseOr :: Maybe a -> Maybe a -> Maybe a
elseOr x y = case y of
              Just _  -> y
              Nothing -> x

-- a wrapper for Choose
data LR = L | R deriving (Show, Read, Eq)
data Choose1 l r a = Choose1 LR (l a) (r a) (Choose l r a) deriving (Show, Read)

data NextNoWrap = NextNoWrap deriving (Eq, Show, Typeable)
instance Message NextNoWrap

handle :: (LayoutClass l a, Message m) => l a -> m -> X (Maybe (l a))
handle l m = handleMessage l (SomeMessage m)

instance (LayoutClass l a, LayoutClass r a) => LayoutClass (Choose1 l r) a where
  description (Choose1 d l r lr) = description lr

  runLayout (Workspace wid (Choose1 d l r lr) s) rec = 
    do 
      (ws, ml0) <- runLayout (Workspace wid lr s) rec
      let l1 = case ml0 of Just l0 -> Just $ Choose1 d l r l0
                           Nothing -> Nothing
      return $ (ws,l1)
    
  handleMessage c@(Choose1 d l r lr) m 
    | Just NextLayout <- fromMessage m = do
        mlr' <- handleMessage lr m
        mlrf <- handle c NextNoWrap
        fstf <- handle c FirstLayout
        let mlf = elseOr fstf mlrf
            (d',l',r') = case mlf of Just (Choose1 d0 l0 r0 lr0) -> (d0,l0,r0)
                                     Nothing                     -> (d,l,r)
        case mlr' of Just lrt -> return $ Just $ Choose1 d' l' r' lrt
                     Nothing  -> return Nothing
    | Just NextNoWrap <- fromMessage m = do
        mlr' <- handleMessage lr m
        (d',l',r') <- 
              case d of 
                L -> do
                       ml <- handle l NextNoWrap
                       case ml of 
                           Just l0 -> return (L,l0,r)
                           Nothing -> do
                                  mr <- handle r FirstLayout
                                  case mr of
                                    Just r0 -> return (R,l,r0)
                                    Nothing -> return (d,l,r)
                R -> do
                       mr <- handle r NextNoWrap
                       case mr of
                         Just r0 -> return (R,l,r0)
                         Nothing -> return (d,l,r)
        case mlr' of Just lrt -> return $ Just $ Choose1 d' l' r' lrt
                     Nothing  -> return Nothing
    | Just FirstLayout <- fromMessage m = do
        mlr' <- handleMessage lr m
        (d',l',r') <- do
                        ml <- handle l FirstLayout
                        case ml of
                          Just l0 -> return (L,l0,r)
                          Nothing -> return (L,l,r)
        case mlr' of Just lrt -> return $ Just $ Choose1 d' l' r' lrt
                     Nothing  -> return Nothing
    | otherwise = do
        mlr' <- handleMessage lr m
        case mlr' of Just lrt -> return $ Just $ Choose1 d l r lrt
                     Nothing  -> return Nothing

(|||) :: l a -> r a -> Choose1 l r a
(|||) l r = Choose1 L l r (l LL.||| r)

-- class for layout combinators
class LayoutClass l a => GetFocused l a where
   getFocused :: l a -> Maybe (Stack a) -> [a]
   getDescription :: l a -> String
   showInfo :: l a -> String

instance (Typeable l, Typeable r, GetFocused l Window, GetFocused r Window) => GetFocused (TMSCombineTwo l r) Window where
  getFocused (TMSCombineTwo f _ _ _ nmaster _ _ lay1 lay2) s =
    let slst = integrate' s
        f' = case s of (Just s') -> focus s':delete (focus s') f
                       Nothing   -> f
        snum = length(slst)
        (slst1, slst2) = splitAt nmaster slst
        s0 = differentiate f' slst
        s1' = differentiate f' slst1
        s2' = differentiate f' slst2
        (s1,s2) | nmaster == 0    = (Nothing,s0)
                | nmaster >= snum = (s0,Nothing)
                | otherwise       = (s1',s2')
        f1 = getFocused lay1 s1
        f2 = getFocused lay2 s2
    in  f1 ++ f2
  getDescription (TMSCombineTwo f _ _ _ nmaster _ _ lay1 lay2) =
    "TMS: " ++ getDescription lay1 ++ "--" ++ getDescription lay2
  showInfo i@(TMSCombineTwo f _ _ _ nmaster _ _ lay1 lay2) =
    "TMS: " ++ description i ++ "--" ++ showInfo lay1 ++ "+" ++ showInfo lay2
  
instance (Typeable l, Typeable r, Typeable a, GetFocused l a, GetFocused r a) => GetFocused (Choose1 l r) a where
  getFocused (Choose1 d l r _) s = 
    case d of L -> getFocused l s
              R -> getFocused r s
  getDescription (Choose1 d l r _) =
    case d of L -> "Choose_L: " ++ getDescription l
              R -> "Choose_R: " ++ getDescription r
  showInfo i@(Choose1 d l r _) = 
      "Choose: " ++ description i ++ "--" ++ showInfo l ++ "+" ++ showInfo r

instance {-# OVERLAPPABLE #-} (Typeable l, Typeable a, LayoutClass l a) => GetFocused l a where
  getFocused l ms =
    case ms of (Just s) -> [focus s]
               Nothing  -> []
  getDescription l = "Base: " ++ description l ++ " type: " ++ show (typeOf l)
  showInfo l = "Base: " ++ description l

-- instance {-# OVERLAPPING #-} (LayoutClass l1 Window, 
--                               LayoutClass l2 Window, 
--                               LayoutClass r1 Window, 
--                               LayoutClass r2 Window)
--     => GetFocused (TMSCombineTwo (TMSCombineTwo l1 r1) (TMSCombineTwo l2 r2)) Window where
--   getFocused = tmsGetFocused
-- instance {-# OVERLAPPING #-} (LayoutClass l1 Window, 
--                               LayoutClass l2 Window, 
--                               LayoutClass r1 Window, 
--                               LayoutClass r2 Window)
--     => GetFocused (TMSCombineTwo (TMSCombineTwo l1 r1) (Choose1 l2 r2)) Window where
--   getFocused = tmsGetFocused
-- instance {-# OVERLAPS #-}    (LayoutClass l1 Window, 
--                               LayoutClass l2 Window, 
--                               LayoutClass r1 Window)
--     => GetFocused (TMSCombineTwo (TMSCombineTwo l1 r1) l2) Window where
--   getFocused = tmsGetFocused
-- instance {-# OVERLAPPING #-} (LayoutClass l1 Window, 
--                               LayoutClass l2 Window, 
--                               LayoutClass r1 Window, 
--                               LayoutClass r2 Window)
--     => GetFocused (TMSCombineTwo (Choose1 l1 r1) (TMSCombineTwo l2 r2)) Window where
--   getFocused = tmsGetFocused
-- instance {-# OVERLAPPING #-} (LayoutClass l1 Window, 
--                               LayoutClass l2 Window, 
--                               LayoutClass r1 Window, 
--                               LayoutClass r2 Window)
--     => GetFocused (TMSCombineTwo (Choose1 l1 r1) (Choose1 l2 r2)) Window where
--   getFocused = tmsGetFocused
-- instance {-# OVERLAPS #-}    (LayoutClass l1 Window, 
--                               LayoutClass l2 Window, 
--                               LayoutClass r1 Window)
--     => GetFocused (TMSCombineTwo (Choose1 l1 r1) l2) Window where
--   getFocused = tmsGetFocused
-- instance {-# OVERLAPS #-}    (LayoutClass l1 Window, 
--                               LayoutClass l2 Window, 
--                               LayoutClass r2 Window)
--     => GetFocused (TMSCombineTwo l1 (TMSCombineTwo l2 r2)) Window where
--   getFocused = tmsGetFocused
-- instance {-# OVERLAPS #-}    (LayoutClass l1 Window, 
--                               LayoutClass l2 Window, 
--                               LayoutClass r2 Window)
--     => GetFocused (TMSCombineTwo l1 (Choose1 l2 r2)) Window where
--   getFocused = tmsGetFocused
-- instance {-# OVERLAPS #-}    (LayoutClass l1 Window, LayoutClass l2 Window)
--     => GetFocused (TMSCombineTwo l1 l2) Window where
--   getFocused = tmsGetFocused
-- 
-- instance {-# OVERLAPS #-}    (LayoutClass l1 Window, LayoutClass l2 Window)
--     => GetFocused (Choose1 l1 l2) Window where
--   getFocused = chsGetFocused

c1' = RowsOrColumns True ||| Simplest
c2' = tmsCombineTwoDefault Simplest Simplest
mylayout1' = tmsCombineTwoDefault c1' c2'
mylayout2' = mylayout1' ||| Simplest 

rows = RowsOrColumns True
myTheme = def { fontName = "xft:DejaVu Sans:size=8" }
myTabbed = tabbed shrinkText myTheme
subLayout = tmsCombineTwoDefault Simplest myTabbed
layout1 = myTabbed ||| subLayout
layout2 = subLayout ||| myTabbed ||| rows
-- *********  Guess: **********
-- Maybe because when called the 
-- GetFocused methods, all the layouts are
-- define to be the superclass instance
-- *********  Guess: **********


-- class LayoutX l a where
--   runLayoutX :: l a -> String
-- 
-- data ChooseX l r a = ChooseX (l a) (r a)
-- instance (LayoutX l a, LayoutX r a) => LayoutX (ChooseX l r) a where
--   runLayoutX _ = "1;"
-- 
-- data TMSX l r a = TMSX (l a) (r a)
-- instance (LayoutX l a, LayoutX r a) => LayoutX (TMSX l r) a where
--   runLayoutX _ = "2;"
-- 
-- data SimplestX a = SimplestX a
-- instance LayoutX SimplestX a where
--   runLayoutX _ = "3;"
-- 
-- data TabbedX a = TabbedX a
-- instance LayoutX TabbedX a where
--   runLayoutX _ = "4;"
-- 
-- class (LayoutX l a) => SaveInfo l a where
--   showInfoX :: l a -> String
-- 
-- instance (SaveInfo l Int, SaveInfo r Int) => SaveInfo (ChooseX l r) Int where
--   showInfoX i@(ChooseX l r) = 
--       "Choose: " ++ runLayoutX i ++ "--" ++ showInfoX l ++ " + " ++ showInfoX r
-- 
-- instance (SaveInfo l Int, SaveInfo r Int) => SaveInfo (TMSX l r) Int where
--   showInfoX i@(TMSX l r) = 
--       "TMS: " ++ runLayoutX i ++ "--" ++ showInfoX l ++ " + " ++ showInfoX r
-- 
-- instance {-# OVERLAPPABLE #-} (LayoutX l a) => SaveInfo l a where
--   showInfoX l = 
--       "Base: " ++ runLayoutX l
-- 
-- 
-- c1 = ChooseX (SimplestX (3::Int)) (SimplestX (3::Int))
-- c2 = TMSX (TabbedX (2::Int)) (TabbedX (2::Int))
-- mylayout1 = TMSX c1 c2
-- mylayout2 = ChooseX (SimplestX (1::Int)) mylayout1 

