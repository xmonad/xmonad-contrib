{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternGuards         #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ScopedTypeVariables   #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.LayoutBuilder
--
-- Copyright   :  (c) 2009 Anders Engstrom <ankaan@gmail.com>,
--                    2011 Ilya Portnov <portnov84@rambler.ru>,
--                    2015 Peter Jones <pjones@devalot.com>
--
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  Anders Engstrom <ankaan@gmail.com>,
--                Ilya Portnov <portnov84@rambler.ru>,
--                Peter Jones <pjones@devalot.com>
--
-- Stability   :  unstable
-- Portability :  unportable
--
-- A layout combinator that sends a specified number of windows to one rectangle
-- and the rest to another. Each of these rectangles are given a layout that
-- is used within them. This can be chained to provide an arbitrary number of
-- rectangles. The layout combinator allows overlapping rectangles, but such
-- layouts does not work well together with hinting
-- ("XMonad.Layout.LayoutHints", "XMonad.Layout.HintedGrid" etc.)
--
-----------------------------------------------------------------------------
module XMonad.Layout.LayoutBuilder (
  -- * Usage
  -- $usage
  layoutN,
  layoutR,
  layoutP,
  layoutAll,

  -- * Selecting Windows
  -- $selectWin
  Predicate (..),
  Proxy(..),

  -- * Messages
  IncLayoutN (..),

  -- * Utilities
  SubMeasure (..),
  SubBox (..),
  absBox,
  relBox,
  LayoutB,
  LayoutN,
) where

--------------------------------------------------------------------------------
import Control.Applicative ((<|>))
import Control.Monad (foldM)
import Data.Maybe
import XMonad
import qualified XMonad.StackSet as W
import XMonad.Util.WindowProperties

--------------------------------------------------------------------------------
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
-- >             ) |||
-- >             ( (layoutN 1 (absBox 10 0 0 (-10)) Nothing $ Tall 0 0.01 0.5)
-- >             $ (layoutN 1 (absBox 0 0 200 0) Nothing $ Tall 0 0.01 0.5)
-- >             $ (layoutAll (absBox 10 10 0 0) $ Tall 2 0.01 0.5)
-- >             ) ||| Full ||| etc...
-- > main = xmonad def { layoutHook = myLayout }
--
-- This will produce a layout similar to DragPane, but with the possibility to have multiple windows in the left half
-- and tabs that show the available windows. It will also produce a layout similar to ThreeColMid and a special layout
-- created for use with a 80 columns wide Emacs window, its sidebar and a tabbed area for all other windows.
--
-- The final layout is for applications that use a toolbar in a separate window, shown on a low resolution screen. It has
-- a master area that cover almost the whole screen. It leaves 10 px to the left and 10 px at the bottom. To the left
-- the toolbar is located and can be accessed by focusing this area. It is actually 200 px wide, but usually below the
-- other windows. Similarly all other windows are tiled, but behind the master window and can be accessed by moving the
-- mouse to the bottom of the screen. Everything can also be accessed by the standard focus changing key bindings.
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

--------------------------------------------------------------------------------
-- $selectWin
--
-- 'Predicate' exists because layouts are required to be serializable, and
-- "XMonad.Util.WindowProperties" is not sufficient (for example it does not
-- allow using regular expressions).
--
-- compare "XMonad.Util.Invisible"

-- | Type class for predicates. This enables us to manage not only Windows,
-- but any objects, for which instance Predicate is defined.
--
-- Another instance exists in XMonad.Util.WindowPropertiesRE in xmonad-extras
class Predicate p w where
  alwaysTrue     :: Proxy w -> p     -- ^ A predicate that is always True.
  checkPredicate :: p -> w -> X Bool -- ^ Check if given object (window or smth else) matches that predicate

instance Predicate () a where
  alwaysTrue _       = ()
  checkPredicate _ _ = return True

instance Predicate Property Window where
  alwaysTrue _   = Const True
  checkPredicate = hasProperty

--------------------------------------------------------------------------------
-- | Contains no actual data, but is needed to help select the correct instance
-- of 'Predicate'
data Proxy a = Proxy

--------------------------------------------------------------------------------
-- | Information about how to split windows between layouts.
data Limit p = LimitN Int                  -- ^ See: 'layoutN'.
             | LimitR (Rational, Rational) -- ^ See: 'layoutR'.
             | LimitP p                    -- ^ See: 'layoutP'.
             deriving (Show, Read)

--------------------------------------------------------------------------------
-- | Use one layout in the specified area for a number of windows and
-- possibly let another layout handle the rest.
data LayoutB l1 l2 p a = LayoutB
    { subFocus  :: Maybe a      -- ^ The focused window in this layout.
    , nextFocus :: Maybe a      -- ^ The focused window in the next layout.
    , limit     :: Limit p      -- ^ How to split windows between layouts.
    , box       :: SubBox       -- ^ Normal size of layout.
    , mbox      :: Maybe SubBox -- ^ Size of layout when handling all windows.
    , sub       :: l1 a         -- ^ The layout to use in this box.
    , next      :: Maybe (l2 a) -- ^ The next layout in the chain.
    } deriving (Show, Read)

--------------------------------------------------------------------------------
-- | A variant of 'LayoutB' that can't use 'layoutP'.  For backwards
-- compatibility with previous versions of LayoutBuilder.
type LayoutN l1 l2 a = LayoutB l1 l2 () a

--------------------------------------------------------------------------------
-- | Use the specified layout in the described area for N windows and
-- send the rest of the windows to the next layout in the chain.  It
-- is possible to supply an alternative area that will then be used
-- instead, if there are no windows to send to the next layout.
layoutN :: (Read a, Eq a, LayoutClass l1 a, LayoutClass l2 a, LayoutClass l3 a) =>
       Int                               -- ^ The number of windows to handle
    -> SubBox                            -- ^ The box to place the windows in
    -> Maybe SubBox                      -- ^ Possibly an alternative box that is used when this layout handles all windows that are left
    -> l1 a                              -- ^ The layout to use in the specified area
    -> LayoutB l2 l3 p a                 -- ^ Where to send the remaining windows
    -> LayoutB l1 (LayoutB l2 l3 p) () a -- ^ The resulting layout
layoutN num box mbox sub next = LayoutB Nothing Nothing (LimitN num) box mbox sub (Just next)

-- | As layoutN, but the number of windows is given relative to the total number of windows remaining to be handled. The first
--   argument is how much to change the ratio when using IncLayoutN, and the second is the initial ratio.
layoutR :: (Read a, Eq a, LayoutClass l1 a, LayoutClass l2 a, LayoutClass l3 a) =>
       Rational                         -- ^ How much to change the ratio with each IncLayoutN
    -> Rational                         -- ^ The ratio of the remaining windows to handle
    -> SubBox                           -- ^ The box to place the windows in
    -> Maybe SubBox                     -- ^ Possibly an alternative box that is used when this layout handles all windows that are left
    -> l1 a                             -- ^ The layout to use in the specified area
    -> LayoutB l2 l3 p a                -- ^ Where to send the remaining windows
    -> LayoutB l1 (LayoutB l2 l3 p) p a -- ^ The resulting layout
layoutR numdiff num box mbox sub next = LayoutB Nothing Nothing (LimitR (numdiff,num)) box mbox sub (Just next)

--------------------------------------------------------------------------------
-- | Use the specified layout in the described area windows that match
-- given predicate and send the rest of the windows to the next layout
-- in the chain.  It is possible to supply an alternative area that
-- will then be used instead, if there are no windows to send to the
-- next layout.
layoutP :: (Read a, Eq a, LayoutClass l1 a, LayoutClass l2 a, LayoutClass l3 a, Predicate p a, Predicate p' a) =>
       p                                 -- ^ The predicate to use
    -> SubBox                            -- ^ The box to place the windows in
    -> Maybe SubBox                      -- ^ Possibly an alternative box that is used when this layout handles all windows that are left
    -> l1 a                              -- ^ The layout to use in the specified area
    -> LayoutB l2 l3 p' a                -- ^ Where to send the remaining windows
    -> LayoutB l1 (LayoutB l2 l3 p') p a -- ^ The resulting layout
layoutP prop box mbox sub next = LayoutB Nothing Nothing (LimitP prop) box mbox sub (Just next)

--------------------------------------------------------------------------------
-- | Use the specified layout in the described area for all remaining windows.
layoutAll :: (Read a, Eq a, LayoutClass l1 a) =>
       SubBox                -- ^ The box to place the windows in
    -> l1 a                  -- ^ The layout to use in the specified area
    -> LayoutB l1 Full () a  -- ^ The resulting layout
layoutAll box sub = LayoutB Nothing Nothing (LimitR (0,1)) box Nothing sub Nothing

--------------------------------------------------------------------------------
-- | Change the number of windows handled by the focused layout.
data IncLayoutN = IncLayoutN Int deriving Typeable
instance Message IncLayoutN

--------------------------------------------------------------------------------
-- | The absolute or relative measures used to describe the area a layout should be placed in. For negative absolute values
--   the total remaining space will be added. For sizes, the remaining space will also be added for zeroes. Relative values
--   are applied on the remaining space after the top-left corner of the box have been removed.
data SubMeasure = Abs Int | Rel Rational deriving (Show,Read)

--------------------------------------------------------------------------------
-- | A box to place a layout in. The stored values are xpos, ypos, width and height.
data SubBox = SubBox SubMeasure SubMeasure SubMeasure SubMeasure deriving (Show,Read)

--------------------------------------------------------------------------------
-- | Create a box with only absolute measurements. If the values are negative, the total remaining space will be added. For
--   sizes it will also be added for zeroes.
absBox :: Int     -- ^ Absolute X-Position
       -> Int     -- ^ Absolute Y-Position
       -> Int     -- ^ Absolute width
       -> Int     -- ^ Absolute height
       -> SubBox  -- ^ The resulting 'SubBox' describing the area
absBox x y w h = SubBox (Abs x) (Abs y) (Abs w) (Abs h)

--------------------------------------------------------------------------------
-- | Create a box with only relative measurements.
relBox :: Rational  -- ^ Relative X-Position with respect to the surrounding area
       -> Rational  -- ^ Relative Y-Position with respect to the surrounding area
       -> Rational  -- ^ Relative width with respect to the remaining width
       -> Rational  -- ^ Relative height with respect to the remaining height
       -> SubBox    -- ^ The resulting 'SubBox' describing the area
relBox x y w h = SubBox (Rel x) (Rel y) (Rel w) (Rel h)

--------------------------------------------------------------------------------
instance ( LayoutClass l1 a, LayoutClass l2 a
         , Read a, Show a, Show p, Typeable p, Eq a, Typeable a, Predicate p a
         ) => LayoutClass (LayoutB l1 l2 p) a where

    -- | Update window locations.
    runLayout (W.Workspace _ LayoutB {..} s) rect = do
        (subs, nexts, subFocus', nextFocus') <- splitStack s limit subFocus nextFocus

        let selBox = if isJust nextFocus' then box else fromMaybe box mbox

        (sublist, sub', schange) <- handle sub subs (calcArea selBox rect)

        (nextlist, next', nchange) <- case next of
          Nothing -> return ([], Nothing, False)
          Just n  -> do (res, l, ch) <- handle n nexts rect
                        return (res, Just l, ch)

        let newlist =  if length (maybe [] W.up s) < length (W.integrate' subs)
                         then sublist++nextlist
                         else nextlist++sublist

            newstate = if subFocus' /= subFocus || nextFocus' /= nextFocus || schange || nchange
                         then Just $ LayoutB subFocus' nextFocus' limit box mbox sub' next'
                         else Nothing

        return (newlist, newstate)
      where
          handle l s' r = do (res,ml) <- runLayout (W.Workspace "" l s') r
                             return (res, fromMaybe l ml, isNothing ml)

    -- |  Propagate messages.
    handleMessage l m
        | Just (IncLayoutN n) <- fromMessage m = incLayoutN l m n
        | Just (IncMasterN _) <- fromMessage m = sendFocus  l m
        | Just Shrink         <- fromMessage m = sendFocus  l m
        | Just Expand         <- fromMessage m = sendFocus  l m
        | otherwise                            = sendBoth   l m

    -- | Descriptive name for layout.
    description layout = case layout of
        (LayoutB _ _ _ _ _ sub Nothing) ->
          "layoutAll " ++ description sub

        (LayoutB _ _ (LimitN _) _ _ sub (Just next)) ->
          "layoutN " ++ description sub ++ " " ++ description next

        (LayoutB _ _ (LimitR _) _ _ sub (Just next)) ->
          "layoutR " ++ description sub ++ " " ++ description next

        (LayoutB _ _ (LimitP _) _ _ sub (Just next)) ->
          "layoutP " ++ description sub ++ " " ++ description next

--------------------------------------------------------------------------------
-- | Increase the number of windows allowed in the focused layout.
incLayoutN :: (LayoutClass l1 a, LayoutClass l2 a, Read a, Show a, Eq a, Typeable a)
           => LayoutB l1 l2 p a
           -> SomeMessage
           -> Int
           -> X (Maybe (LayoutB l1 l2 p a))
incLayoutN layout@LayoutB {..} message n = do
    incThis <- isFocus subFocus

    if incThis
       then return $ Just layout { limit = newLimit }
       else sendNext layout message

  where
    newLimit = case limit of
      LimitN oldnum         -> LimitN (max 1 $ oldnum + n)
      LimitR (diff, oldnum) -> LimitR (diff, min 1 $ max 0 $ oldnum + fromIntegral n * diff)
      LimitP _              -> limit

--------------------------------------------------------------------------------
sendSub :: (LayoutClass l1 a, LayoutClass l2 a, Read a, Show a, Eq a, Typeable a) => LayoutB l1 l2 p a -> SomeMessage -> X (Maybe (LayoutB l1 l2 p a))
sendSub (LayoutB subFocus nextFocus num box mbox sub next) m =
    do sub' <- handleMessage sub m
       return $ if isJust sub'
                then Just $ LayoutB subFocus nextFocus num box mbox (fromMaybe sub sub') next
                else Nothing

--------------------------------------------------------------------------------
sendBoth :: (LayoutClass l1 a, LayoutClass l2 a, Read a, Show a, Eq a, Typeable a) => LayoutB l1 l2 p a -> SomeMessage -> X (Maybe (LayoutB l1 l2 p a))
sendBoth l@(LayoutB _ _ _ _ _ _ Nothing) m = sendSub l m
sendBoth (LayoutB subFocus nextFocus num box mbox sub (Just next)) m =
    do sub' <- handleMessage sub m
       next' <- handleMessage next m
       return $ if isJust sub' || isJust next'
                then Just $ LayoutB subFocus nextFocus num box mbox (fromMaybe sub sub') (next' <|> Just next)
                else Nothing

--------------------------------------------------------------------------------
sendNext :: (LayoutClass l1 a, LayoutClass l2 a, Read a, Show a, Eq a, Typeable a) => LayoutB l1 l2 p a -> SomeMessage -> X (Maybe (LayoutB l1 l2 p a))
sendNext (LayoutB _ _ _ _ _ _ Nothing) _ = return Nothing
sendNext (LayoutB subFocus nextFocus num box mbox sub (Just next)) m =
    do next' <- handleMessage next m
       return $ if isJust next'
                then Just $ LayoutB subFocus nextFocus num box mbox sub next'
                else Nothing

--------------------------------------------------------------------------------
sendFocus :: (LayoutClass l1 a, LayoutClass l2 a, Read a, Show a, Eq a, Typeable a) => LayoutB l1 l2 p a -> SomeMessage -> X (Maybe (LayoutB l1 l2 p a))
sendFocus l@(LayoutB subFocus _ _ _ _ _ _) m = do
  foc <- isFocus subFocus

  if foc
    then sendSub  l m
    else sendNext l m

--------------------------------------------------------------------------------
-- | Check to see if the given window is currently focused.
isFocus :: (Show a) => Maybe a -> X Bool
isFocus Nothing = return False
isFocus (Just w) = do ms <- (W.stack . W.workspace . W.current) <$> gets windowset
                      return $ maybe False (\s -> show w == show (W.focus s)) ms

--------------------------------------------------------------------------------
calcNum :: Int -> Limit p -> Int
calcNum tot num = max 1 $ case num of LimitN i     -> i
                                      LimitR (_,r) -> ceiling $ r * fromIntegral tot
                                      LimitP _     -> 1

--------------------------------------------------------------------------------
-- | Split given list of objects (i.e. windows) using predicate.
splitBy :: (Predicate p a) => p -> [a] -> X ([a], [a])
splitBy prop = foldM step ([], [])
  where
    step (good, bad) w = do
      ok <- checkPredicate prop w
      return $ if ok
                then (w:good, bad)
                else (good,   w:bad)

--------------------------------------------------------------------------------
splitStack :: forall a p. (Eq a, Predicate p a)
           => Maybe (W.Stack a) -- ^ Window set.
           -> Limit p           -- ^ How to split the stack.
           -> Maybe a           -- ^ The window that was focused in this layout.
           -> Maybe a           -- ^ The window that was focused in the next layout.
           -> X (Maybe (W.Stack a), Maybe (W.Stack a), Maybe a, Maybe a)
splitStack Nothing _ _ _ = return (Nothing, Nothing, Nothing, Nothing)
splitStack (Just s) limit subFocus nextFocus =
  case limit of
    LimitN _    -> splitN
    LimitR _    -> splitN
    LimitP prop -> splitP prop

  where
    ws        = W.integrate s
    n         = calcNum (length ws) limit
    subl      = take n ws
    nextl     = drop n ws
    subFocus' xs  = foc xs subFocus
    nextFocus' xs = foc xs nextFocus

    -- Pick a new focused window if necessary.
    foc :: [a] -> Maybe a -> Maybe a
    foc [] _                           = Nothing
    foc l f | W.focus s `elem` l       = Just (W.focus s)
            | maybe False (`elem` l) f = f
            | otherwise                = listToMaybe l

    -- Split based on max number of windows.
    splitN = return ( differentiate' (subFocus' subl)   subl
                    , differentiate' (nextFocus' nextl) nextl
                    , subFocus'  subl
                    , nextFocus' nextl
                    )

    -- Split based on a predicate.
    splitP prop = do
      (this, other) <- splitBy prop ws
      return ( differentiate' (subFocus' this)   this
             , differentiate' (nextFocus' other) other
             , subFocus' this
             , nextFocus' other
             )

--------------------------------------------------------------------------------
calcArea :: SubBox -> Rectangle -> Rectangle
calcArea (SubBox xpos ypos width height) rect =
    Rectangle (rect_x rect + fromIntegral xpos')
              (rect_y rect + fromIntegral ypos')
              width' height'
  where
    xpos' = calc False xpos $ rect_width rect
    ypos' = calc False ypos $ rect_height rect
    width' = calc True width $ rect_width rect - xpos'
    height' = calc True height $ rect_height rect - ypos'

    calc zneg val tot = fromIntegral $ min (fromIntegral tot) $ max 0 $
        case val of Rel v -> floor $ v * fromIntegral tot
                    Abs v -> if v<0 || (zneg && v==0)
                               then fromIntegral tot + v
                               else v

--------------------------------------------------------------------------------
differentiate' :: Eq q => Maybe q -> [q] -> Maybe (W.Stack q)
differentiate' _ [] = Nothing
differentiate' Nothing w = W.differentiate w
differentiate' (Just f) w
    | f `elem` w = Just W.Stack { W.focus = f
                                , W.up    = reverse $ takeWhile (/=f) w
                                , W.down  = tail $ dropWhile (/=f) w
                                }
    | otherwise = W.differentiate w
