{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances, PatternGuards, DeriveDataTypeable #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.LayoutBuilder
-- Copyright   :  (c) 2009 Anders Engstrom <ankaan@gmail.com>
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  Anders Engstrom <ankaan@gmail.com>
-- Stability   :  unstable
-- Portability :  unportable
--
-- A layout combinator that sends a specified number of windows to one rectangle
-- and the rest to another.
--
-----------------------------------------------------------------------------

module XMonad.Layout.LayoutBuilder (
  -- * Usage
  -- $usage
  layoutN,
  layoutR,
  layoutAll,
  IncLayoutN (..),
  SubMeasure (..),
  SubBox (..),
  absBox,
  relBox,
  LayoutN,
) where

import XMonad
import qualified XMonad.StackSet as W
import Data.Maybe (isJust)

-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Layout.LayoutBuilder
--
-- Then edit your @layoutHook@ by adding something like:
--
-- > myLayout = ( (layoutN 1 (relBox 0 0 0.5 1) (Just $ relBox 0 0 1 1) $ simpleTabbed)
-- >             $ (layoutAll (relBox 0.5 0 1 1)                         $ simpleTabbed)
-- >             ) |||
-- >             ( (layoutN 1       (relBox (1/3) 0 (1/2) 1) (Just $ relBox 0 0 1 1) $ Tall 0 0.01 0.5)
-- >             $ (layoutR 0.1 0.5 (relBox (2/3) 0 1     1) Nothing                 $ Tall 0 0.01 0.5)
-- >             $ (layoutAll       (relBox 0     0 (1/3) 1)                         $ Tall 0 0.01 0.5)
-- >             ) |||
-- >             ( (layoutN 1 (absBox (-512-200) 0 512        0) (Just $ relBox 0 0 1 1) $ simpleTabbed)
-- >             $ (layoutN 1 (absBox (-200)     0 0          0) Nothing                 $ simpleTabbed)
-- >             $ (layoutAll (absBox 0          0 (-512-200) 0)                         $ simpleTabbed)
-- >             ) ||| Full ||| etc...
-- > main = xmonad def { layoutHook = myLayout }
--
-- This will produce a layout similar to DragPane, but with the possibility to have multiple windows in the left half
-- and tabs that show the available windows. It will also produce a layout similar to ThreeColMid and a special layout
-- created for use with a 80 columns wide Emacs window, its sidebar and a tabbed area for all other windows.
--
-- This module can be used to create many different custom layouts, but there are limitations. The primary limitation
-- can be observed in the second and third example when there are only two columns with windows in them. The leftmost
-- area is left blank. These blank areas can be avoided by placing the rectangles appropriately.
--
-- These examples require "XMonad.Layout.Tabbed".
--
-- For more detailed instructions on editing the layoutHook see:
--
-- "XMonad.Doc.Extending#Editing_the_layout_hook"
--
-- You may wish to add the following keybindings:
--
-- >    , ((modm .|. shiftMask, xK_h ), sendMessage $ IncLayoutN (-1))
-- >    , ((modm .|. shiftMask, xK_l ), sendMessage $ IncLayoutN 1)
--
-- For detailed instruction on editing the key binding see:
--
-- "XMonad.Doc.Extending#Editing_key_bindings".

type WindowNum = Either Int (Rational,Rational)

-- | Use one layout in the specified area for a number of windows and possibly let another layout handle the rest.
data LayoutN l1 l2 a =
    LayoutN (Maybe a) (Maybe a) WindowNum SubBox (Maybe SubBox) (l1 a) (Maybe (l2 a))
    deriving (Show,Read)

-- | Use the specified layout in the described area for N windows and send the rest of the windows to the next layout in the chain.
--   It is possible to supply an alternative area that will then be used instead, if there are no windows to send to the next layout.
layoutN :: (Read a, Eq a, LayoutClass l1 a, LayoutClass l2 a, LayoutClass l3 a) =>
       Int                          -- ^ The number of windows to handle
    -> SubBox                       -- ^ The box to place the windows in
    -> Maybe SubBox                 -- ^ Possibly an alternative box that is used when this layout handles all windows that are left
    -> l1 a                         -- ^ The layout to use in the specified area
    -> LayoutN l2 l3 a              -- ^ Where to send the remaining windows
    -> LayoutN l1 (LayoutN l2 l3) a -- ^ The resulting layout
layoutN num box mbox sub next = LayoutN Nothing Nothing (Left num) box mbox sub (Just next)

-- | As layoutN, but the number of windows is given relative to the total number of windows remaining to be handled. The first
--   argument is how much to change the ratio when using IncLayoutN, and the second is the initial ratio.
layoutR :: (Read a, Eq a, LayoutClass l1 a, LayoutClass l2 a, LayoutClass l3 a) =>
       Rational                     -- ^ How much to change the ratio with each IncLayoutN
    -> Rational                     -- ^ The ratio of the remaining windows to handle
    -> SubBox                       -- ^ The box to place the windows in
    -> Maybe SubBox                 -- ^ Possibly an alternative box that is used when this layout handles all windows that are left
    -> l1 a                         -- ^ The layout to use in the specified area
    -> LayoutN l2 l3 a              -- ^ Where to send the remaining windows
    -> LayoutN l1 (LayoutN l2 l3) a -- ^ The resulting layout
layoutR numdiff num box mbox sub next = LayoutN Nothing Nothing (Right (numdiff,num)) box mbox sub (Just next)

-- | Use the specified layout in the described area for all remaining windows.
layoutAll :: (Read a, Eq a, LayoutClass l1 a) =>
       SubBox             -- ^ The box to place the windows in
    -> l1 a               -- ^ The layout to use in the specified area
    -> LayoutN l1 Full a  -- ^ The resulting layout
layoutAll box sub = LayoutN Nothing Nothing (Right (0,1)) box Nothing sub Nothing

-- | Change the number of windows handled by the focused layout.
data IncLayoutN = IncLayoutN Int deriving Typeable
instance Message IncLayoutN

-- | The absolute or relative measures used to describe the area a layout should be placed in. For negative absolute values
--   the total remaining space will be added. For sizes, the remaining space will also be added for zeroes. Relative values
--   are applied on the remaining space after the top-left corner of the box have been removed.
data SubMeasure = Abs Int | Rel Rational deriving (Show,Read)

-- | A box to place a layout in. The stored values are xpos, ypos, width and height.
data SubBox = SubBox SubMeasure SubMeasure SubMeasure SubMeasure deriving (Show,Read)


-- | Create a box with only absolute measurements. If the values are negative, the total remaining space will be added. For
--   sizes it will also be added for zeroes.
absBox :: Int     -- ^ Absolute X-Position
       -> Int     -- ^ Absolute Y-Position
       -> Int     -- ^ Absolute width
       -> Int     -- ^ Absolute height
       -> SubBox  -- ^ The resulting 'SubBox' describing the area
absBox x y w h = SubBox (Abs x) (Abs y) (Abs w) (Abs h)


-- | Create a box with only relative measurements.
relBox :: Rational  -- ^ Relative X-Position with respect to the surrounding area
       -> Rational  -- ^ Relative Y-Position with respect to the surrounding area
       -> Rational  -- ^ Relative width with respect to the remaining width
       -> Rational  -- ^ Relative height with respect to the remaining height
       -> SubBox    -- ^ The resulting 'SubBox' describing the area
relBox x y w h = SubBox (Rel x) (Rel y) (Rel w) (Rel h)


instance (LayoutClass l1 a, LayoutClass l2 a, Read a, Show a, Eq a, Typeable a) =>
    LayoutClass (LayoutN l1 l2) a where

        -- | Update window locations.
        runLayout (W.Workspace _ (LayoutN subf nextf num box mbox sub next) s) rect
            = do let (subs,nexts,subf',nextf') = splitStack s num subf nextf
                     selBox = if isJust nextf'
                              then box
                              else maybe box id mbox

                 (sublist,sub') <- handle sub subs $ calcArea selBox rect

                 (nextlist,next') <- case next of Nothing -> return ([],Nothing)
                                                  Just n -> do (res,l) <- handle n nexts rect
                                                               return (res,Just l)

                 return (sublist++nextlist, Just $ LayoutN subf' nextf' num box mbox sub' next' )
              where
                  handle l s' r = do (res,ml) <- runLayout (W.Workspace "" l s') r
                                     l' <- return $ maybe l id ml
                                     return (res,l')

        -- |  Propagate messages.
        handleMessage l m
            | Just (IncLayoutN _) <- fromMessage m = windowNum l m
            | Just (IncMasterN _) <- fromMessage m = sendFocus l m
            | Just (Shrink) <- fromMessage m = sendFocus l m
            | Just (Expand) <- fromMessage m = sendFocus l m
            | otherwise = sendBoth l m

        -- |  Descriptive name for layout.
        description (LayoutN _ _ _ _ _ sub Nothing) = "layoutAll "++ description sub
        description (LayoutN _ _ (Left _) _ _ sub (Just next)) = "layoutN "++ description sub ++" "++ description next
        description (LayoutN _ _ (Right _) _ _ sub (Just next)) = "layoutR "++ description sub ++" "++ description next


windowNum :: (LayoutClass l1 a, LayoutClass l2 a, Read a, Show a, Eq a, Typeable a) => LayoutN l1 l2 a -> SomeMessage -> X (Maybe (LayoutN l1 l2 a))
windowNum l@(LayoutN subf nextf num box mbox subl nextl) m | (Just (IncLayoutN n)) <- fromMessage m =
    do foc <- isFocus subf
       if foc then do let newnum = case num of
                                       (Left oldnum) -> Left $ max 1 $ oldnum + n
                                       (Right (diff,oldnum)) -> Right (diff, min 1 $ max 0 $ oldnum + (fromIntegral n)*diff)
                      return $ Just $ LayoutN subf nextf newnum box mbox subl nextl
              else sendNext l m
windowNum l m = sendNext l m

sendSub :: (LayoutClass l1 a, LayoutClass l2 a, Read a, Show a, Eq a, Typeable a) => LayoutN l1 l2 a -> SomeMessage -> X (Maybe (LayoutN l1 l2 a))
sendSub (LayoutN subf nextf num box mbox sub next) m =
    do sub' <- handleMessage sub m
       return $ if isJust sub'
                then Just $ LayoutN subf nextf num box mbox (maybe sub id sub') next
                else Nothing

sendBoth :: (LayoutClass l1 a, LayoutClass l2 a, Read a, Show a, Eq a, Typeable a) => LayoutN l1 l2 a -> SomeMessage -> X (Maybe (LayoutN l1 l2 a))
sendBoth l@(LayoutN _ _ _ _ _ _ Nothing) m = sendSub l m
sendBoth (LayoutN subf nextf num box mbox sub (Just next)) m =
    do sub' <- handleMessage sub m
       next' <- handleMessage next m
       return $ if isJust sub' || isJust next'
                then Just $ LayoutN subf nextf num box mbox (maybe sub id sub') (Just $ maybe next id next')
                else Nothing

sendNext :: (LayoutClass l1 a, LayoutClass l2 a, Read a, Show a, Eq a, Typeable a) => LayoutN l1 l2 a -> SomeMessage -> X (Maybe (LayoutN l1 l2 a))
sendNext (LayoutN _ _ _ _ _ _ Nothing) _ = return Nothing
sendNext (LayoutN subf nextf num box mbox sub (Just next)) m =
    do next' <- handleMessage next m
       return $ if isJust next'
                then Just $ LayoutN subf nextf num box mbox sub next'
                else Nothing

sendFocus :: (LayoutClass l1 a, LayoutClass l2 a, Read a, Show a, Eq a, Typeable a) => LayoutN l1 l2 a -> SomeMessage -> X (Maybe (LayoutN l1 l2 a))
sendFocus l@(LayoutN subf _ _ _ _ _ _) m = do foc <- isFocus subf
                                              if foc then sendSub l m
                                                     else sendNext l m

isFocus :: (Show a) => Maybe a -> X Bool
isFocus Nothing = return False
isFocus (Just w) = do ms <- (W.stack . W.workspace . W.current) `fmap` gets windowset
                      return $ maybe False (\s -> show w == (show $ W.focus s)) ms


calcNum :: Int -> WindowNum -> Int
calcNum tot num = max 1 $ case num of Left i -> i
                                      Right (_,r) -> ceiling $ r * fromIntegral tot

splitStack :: Eq a => Maybe (W.Stack a) -> WindowNum -> Maybe a -> Maybe a -> (Maybe (W.Stack a),Maybe (W.Stack a),Maybe a,Maybe a)
splitStack Nothing _ _ _ = (Nothing,Nothing,Nothing,Nothing)
splitStack (Just s) num subf nextf = ( differentiate' subf' subl
                                     , differentiate' nextf' nextl
                                     , subf'
                                     , nextf'
                                     )
    where
        ws = W.integrate s
        n = calcNum (length ws) num
        subl = take n ws
        nextl = drop n ws
        subf' = foc subl subf
        nextf' = foc nextl nextf
        foc [] _ = Nothing
        foc l f = if W.focus s `elem` l
                  then Just $ W.focus s
                  else if maybe False (`elem` l) f
                       then f
                       else Just $ head l

calcArea :: SubBox -> Rectangle -> Rectangle
calcArea (SubBox xpos ypos width height) rect = Rectangle (rect_x rect + fromIntegral xpos') (rect_y rect + fromIntegral ypos') width' height'
    where
        xpos' = calc False xpos $ rect_width rect
        ypos' = calc False ypos $ rect_height rect
        width' = calc True width $ rect_width rect - xpos'
        height' = calc True height $ rect_height rect - ypos'

        calc zneg val tot = fromIntegral $ min (fromIntegral tot) $ max 0 $
            case val of Rel v -> floor $ v * fromIntegral tot
                        Abs v -> if v<0 || (zneg && v==0)
                                 then (fromIntegral tot)+v
                                 else v

differentiate' :: Eq q => Maybe q -> [q] -> Maybe (W.Stack q)
differentiate' _ [] = Nothing
differentiate' Nothing w = W.differentiate w
differentiate' (Just f) w
    | f `elem` w = Just $ W.Stack { W.focus = f
                                  , W.up    = reverse $ takeWhile (/=f) w
                                  , W.down  = tail $ dropWhile (/=f) w
                                  }
    | otherwise = W.differentiate w
