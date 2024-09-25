{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

-- |
-- Module: XMonad.Layout.Columns
-- Description: A layout which tiles the windows in columns.
-- Copyright: Jean-Charles Quillet
-- License: BSD-style (see LICENSE)
--
-- Maintainer: none
-- Stability: unstable
-- Portability: unportable
--
-- A layout which tiles the windows in columns. The windows can be moved and
-- resized in every directions.
--
-- The first window appears in a single column in the center of the screen. Its
-- width is configurable (See 'coOneWindowWidth').
--
-- The second window appears in a second column. Starting with two columns, they
-- fill up the screen.
--
-- Subsequent windows appear on the bottom of the last columns.
module XMonad.Layout.Columns
  ( -- * Usage
    -- $usage
    ColumnsLayout (..),

    -- * Messages
    Focus (..),
    Move (..),
    Resize (..),

    -- * Tools
    focusDown,
    focusUp,
  )
where

import Control.Applicative ((<|>))
import Control.Arrow (Arrow (first), second)
import Control.Monad (guard)
import Control.Monad.State (modify)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.Foldable (Foldable (..))
import Data.List (scanl')
import Data.Maybe (listToMaybe)
import Data.Ratio ((%))
import XMonad
  ( LayoutClass (..),
    Message,
    Rectangle (..),
    SomeMessage,
    Window,
    WindowSet,
    X,
    XState (..),
    fromMessage,
    gets,
    scaleRationalRect,
    sendMessage,
  )
import qualified XMonad.Operations as O
import XMonad.StackSet
  ( RationalRect (..),
    Screen (..),
    Stack (..),
    StackSet (..),
    integrate,
    peek,
  )
import qualified XMonad.StackSet as StackSet

-- $usage
-- Add 'Columns' to your @layoutHook@ with an initial empty state:
--
-- > myLayout = Full ||| Columns 1 []
--
-- Here is an example of keybindings:
--
-- > -- Focus up/down
-- > ((modm, xK_Tab), focusDown),
-- > ((modm .|. shiftMask, xK_Tab), focusUp),
-- > -- Move windows around
-- > ((modm .|. shiftMask, xK_l), sendMessage MoveRight),
-- > ((modm .|. shiftMask, xK_h), sendMessage MoveLeft),
-- > ((modm .|. shiftMask, xK_k), sendMessage MoveUp),
-- > ((modm .|. shiftMask, xK_j), sendMessage MoveDown),
-- > -- Resize them
-- > ((modm .|. controlMask, xK_l), sendMessage HorizontalExpand),
-- > ((modm .|. controlMask, xK_h), sendMessage HorizontalShrink),
-- > ((modm .|. controlMask, xK_k), sendMessage VerticalExpand),
-- > ((modm .|. controlMask, xK_j), sendMessage VerticalShrink),
--
-- This layout is known to work with:
--
-- * "XMonad.Layout.WindowNavigation" for changing focus with a direction using
-- 'XMonad.Layout.WindowNavigation.Go' messages.
-- * 'XMonad.Layout.SubLayouts.subTabbed' for docking windows together with
-- tabs. Note that sometimes when undocking windows, the layout is reset. This is
-- a minor annoyance caused by the difficulty to track windows in the sublayout.

-- | The windows can be moved in every directions.
--
-- Horizontally, a window alone in its column cannot be moved before the first
-- or after the last column. If not alone, moving the window outside those
-- limits will create a new column.
-- The windows can also be moved vertically in their column.
data Move = MoveLeft | MoveRight | MoveUp | MoveDown deriving (Show, Read)

instance Message Move

-- | The windows can be resized in every directions.
--
-- When resizing horizontally:
--
-- * if the window to be resized is not in the last column
--
--      * then the right side of the window will be moved
--      * the last column will compensate the size change
--
-- * if the window is in the last column
--
--      * then the left side of the window will be moved
--      * the column on the left of the current one will compensate the size change
--
-- The same applies when resizing vertically using the bottom side of the
-- window unless it is the last window in the column in which case we use the
-- top side.
data Resize
  = VerticalShrink
  | VerticalExpand
  | HorizontalShrink
  | HorizontalExpand
  deriving (Show, Read)

instance Message Resize

-- | The layout handles focus change messages.
--
-- Built-in focus cannot be used here because @XMonad@ does not make it easy to
-- change the order of windows in the focus list. See also 'focusUp' and
-- 'focusDown' functions.
data Focus = FocusUp | FocusDown
  deriving (Show, Read)

instance Message Focus

-- | A column is a list of windows with their relative vertical dimensions.
type Column = [(Rational, Window)]

-- | The layout is a list of 'Column' with their relative horizontal dimensions.
type Columns = [(Rational, Column)]

data ColumnsLayout a = Columns
  { -- | With of the first column when there is only one window. Usefull on wide
    -- screens.
    coOneWindowWidth :: Rational,
    -- | The current state
    coColumns :: Columns
  }
  deriving (Show, Read)

instance LayoutClass ColumnsLayout Window where
  description _ = layoutDescription

  doLayout (Columns oneWindowWidth columns) rectangle stack =
    pure (rectangles, Just (Columns oneWindowWidth columns'))
    where
      hackedColumns = hackForTabs columns stack
      columns' = updateWindowList hackedColumns stack
      rectangles = toRectangles rectangle' columns'
      -- If there is only one window, we set the destination rectangle according
      -- to the width in the layout setting.
      rectangle'
        | (length . toList $ stack) == 1 =
            scaleRationalRect rectangle singleColumnRR
        | otherwise = rectangle
      singleColumnOffset = (1 - oneWindowWidth) / 2
      singleColumnRR = RationalRect singleColumnOffset 0 oneWindowWidth 1

  handleMessage layout@(Columns oneWindowWidth columns) message = do
    mbStack <- runMaybeT $ handleFocus' =<< getStack
    changedFocus <- traverse updateStack' mbStack

    movedOrResized <-
      runMaybeT $
        Columns oneWindowWidth
          <$> (handleMoveOrResize' =<< peekFocus)

    pure $ movedOrResized <|> changedFocus
    where
      getStack = MaybeT . gets $ StackSet.stack . workspace . current . windowset
      handleFocus' = hoistMaybe . handleFocus columns message
      -- A 'Just' needs to be return for the new stack to be taken into account
      updateStack' s = modify (setStack s) >> pure layout
      peekFocus = MaybeT . gets $ peek . windowset
      handleMoveOrResize' = hoistMaybe . handleMoveOrResize columns message
      hoistMaybe = MaybeT . pure

layoutDescription :: String
layoutDescription = "Columns"

-- | Change the keyboard focus to the previous window
focusUp :: X ()
focusUp =
  sendMsgOrOnWindowsSet FocusUp StackSet.focusUp
    =<< getCurrentLayoutDescription

-- | Change the keyboard focus to the next window
focusDown :: X ()
focusDown =
  sendMsgOrOnWindowsSet FocusDown StackSet.focusDown
    =<< getCurrentLayoutDescription

sendMsgOrOnWindowsSet :: (Message a) => a -> (WindowSet -> WindowSet) -> String -> X ()
sendMsgOrOnWindowsSet message f description'
  | description' == layoutDescription = sendMessage message
  | otherwise = O.windows f

getCurrentLayoutDescription :: X String
getCurrentLayoutDescription =
  gets
    ( description
        . StackSet.layout
        . workspace
        . current
        . windowset
    )

setStack :: Stack Window -> XState -> XState
setStack stack state =
  state
    { windowset =
        (windowset state)
          { current =
              (current $ windowset state)
                { workspace =
                    (workspace . current $ windowset state)
                      { StackSet.stack = Just stack
                      }
                }
          }
    }

handleFocus :: Columns -> SomeMessage -> Stack Window -> Maybe (Stack Window)
handleFocus columns message stack
  | Just FocusDown <- fromMessage message = setFocus' stack <$> mbNext
  | Just FocusUp <- fromMessage message = setFocus' stack <$> mbPrevious
  | otherwise = Nothing
  where
    focused = focus stack
    windows = columnsToWindows columns
    exists = focused `elem` windows
    mbNext = guard exists >> next focused windows
    mbPrevious = guard exists >> previous focused windows
    setFocus' = flip setFocus
    previous a = next a . reverse
    setFocus w = until ((==) w . focus) StackSet.focusDown'
    next _ [] = Nothing
    next a (x : xs)
      | a == x = listToMaybe xs
      | otherwise = next a (xs <> [x])

oldNewWindows :: Columns -> Stack Window -> ([Window], [Window])
oldNewWindows columns stack = (old, new)
  where
    old = filter (`notElem` stackList) windows
    new = filter (`notElem` windows) stackList
    stackList = toList stack
    windows = columnsToWindows columns

-- | Add the new windows to the layout and remove the old ones.
updateWindowList :: Columns -> Stack Window -> Columns
updateWindowList columns stack = addWindows newWindows (removeWindows oldWindows columns)
  where
    (oldWindows, newWindows) = oldNewWindows columns stack

-- | If one window disappeared and another appeared, we assume that the sublayout
-- tabs just changed focused.
hackForTabs :: Columns -> Stack Window -> Columns
hackForTabs columns stack = mapWindow replace columns
  where
    replace window
      | (w1 : _, [w2]) <- oldNewWindows columns stack =
          if window == w1
            then w2
            else window
      | otherwise = window

toRectangles :: Rectangle -> [(Rational, [(Rational, a)])] -> [(a, Rectangle)]
toRectangles rectangle columns =
  second (scaleRationalRect rectangle) <$> windowsAndRectangles
  where
    offsetsAndRatios = toOffsetRatio (second toOffsetRatio <$> columns)
    windowsAndRectangles = foldMap toWindowAndRectangle offsetsAndRatios
    toWindowAndRectangle (x, w, cs) = (\(y, h, ws) -> (ws, RationalRect x y w h)) <$> cs

onFocused :: (a -> a) -> Stack a -> Stack a
onFocused f (Stack a before after) = Stack (f a) before after

onFocusedM :: (Monad m) => (a -> m a) -> Stack a -> m (Stack a)
onFocusedM f (Stack a before after) = Stack <$> f a <*> pure before <*> pure after

onFocusedOrPrevious :: (a -> a) -> Stack a -> Stack a
onFocusedOrPrevious f (Stack a (a' : others) []) = Stack a (f a' : others) []
onFocusedOrPrevious f stack = onFocused f stack

handleMoveOrResize :: Columns -> SomeMessage -> Window -> Maybe Columns
handleMoveOrResize columns message window
  | Just msg <- fromMessage message = move msg window columns
  | Just HorizontalShrink <- fromMessage message =
      onFocusedOrPrevious' shrink <$> findInColumns window columns
  | Just HorizontalExpand <- fromMessage message =
      onFocusedOrPrevious' expand <$> findInColumns window columns
  | Just VerticalExpand <- fromMessage message =
      onFocusedM'
        (fmap (onFocusedOrPrevious' shrink) . findInColumn window)
        =<< findInColumns window columns
  | Just VerticalShrink <- fromMessage message =
      onFocusedM'
        (fmap (onFocusedOrPrevious' expand) . findInColumn window)
        =<< findInColumns window columns
  | otherwise = Nothing
  where
    expand = first $ flip (+) (3 / 100)
    shrink = first $ flip (-) (3 / 100)
    onFocusedM' f = fmap integrate . onFocusedM (sequence . second f)
    onFocusedOrPrevious' f = sanitize . integrate . onFocusedOrPrevious f

move :: Move -> Window -> Columns -> Maybe Columns
move direction window columns =
  case (direction, findInColumns window columns) of
    (MoveRight, Just (Stack (_, [(_, _)]) _ [])) -> Nothing
    (MoveLeft, Just (Stack (_, [(_, _)]) [] _)) -> Nothing
    (MoveRight, Just (Stack column@(_, [(_, _)]) before (next : others))) ->
      let (column', next') = swapWindowBetween window column next
       in Just . integrate $ Stack column' before (next' : others)
    (MoveLeft, Just (Stack column@(_, [(_, _)]) (previous : others) after)) ->
      let (column', previous') = swapWindowBetween window column previous
       in Just . integrate $ Stack column' (previous' : others) after
    (MoveRight, Just stack) ->
      let (newColumns', Stack column before after) = rationalize newColumns stack
          windows = removeWindow window column
       in Just . integrate $ Stack windows before (newColumns' <> after)
    (MoveLeft, Just stack) ->
      let (newColumns', Stack column before after) = rationalize newColumns stack
          windows = removeWindow window column
       in Just . integrate $ Stack windows (newColumns' <> before) after
    (MoveUp, Just stack) -> integrate <$> onFocusedM (swapWindowUp window) stack
    (MoveDown, Just stack) -> integrate <$> onFocusedM (swapWindowDown window) stack
    _ -> Nothing
  where
    newColumns = [[(1, window)]]

mapWindow :: (Window -> Window) -> Columns -> Columns
mapWindow = fmap . fmap . fmap . fmap

columnsToWindows :: Columns -> [Window]
columnsToWindows = foldMap ((: []) . snd) . foldMap snd

swapWindowBetween ::
  Window ->
  (Rational, Column) ->
  (Rational, Column) ->
  ((Rational, Column), (Rational, Column))
swapWindowBetween window from to = (removed, added)
  where
    removed = removeWindow window from
    added = appendWindows [window] to

swapWindowUp :: Window -> (Rational, Column) -> Maybe (Rational, Column)
swapWindowUp window (width, column)
  | Just (Stack (height, _) (previous : before') after) <- findInColumn window column =
      Just (width, integrate $ Stack previous ((height, window) : before') after)
  | otherwise = Nothing

swapWindowDown :: Window -> (Rational, Column) -> Maybe (Rational, Column)
swapWindowDown window (width, column)
  | Just (Stack (height, _) before (next : others)) <- findInColumn window column =
      Just (width, integrate $ Stack next before ((height, window) : others))
  | otherwise = Nothing

-- | Adjust the ratio of a list or a stack of elts so that when adding new
--  elements:
-- - the new elements are distributed according to the total number of elements
-- - the existing elements keep their proportion in the remaining space
rationalize ::
  (Functor f, Foldable f) =>
  [a] ->
  f (Rational, a) ->
  ([(Rational, a)], f (Rational, a))
rationalize new existing = (new', existing')
  where
    nbNew = fromIntegral $ length new
    nbInColumn = fromIntegral $ length existing
    newRatio = nbNew % (nbNew + nbInColumn)
    existingRatio = 1 - newRatio
    new' = fitElements newRatio new
    existing' = first (* existingRatio) <$> existing

append :: [a] -> [(Rational, a)] -> [(Rational, a)]
append new existing = uncurry (flip mappend) (rationalize new existing)

appendWindows ::
  [Window] ->
  (Rational, [(Rational, Window)]) ->
  (Rational, [(Rational, Window)])
appendWindows windows = second (append windows)

fitElements :: Rational -> [a] -> [(Rational, a)]
fitElements dimension elts = (dimension',) <$> elts
  where
    dimension' = dimension / fromIntegral (length elts)

singleColumn :: Rational -> Rational -> [Window] -> Columns
singleColumn width height windows = [(width, fitElements height windows)]

findElement' :: (a -> Bool) -> [(Rational, a)] -> Maybe (Stack (Rational, a))
findElement' predicate list
  | (before, c : after) <- break (predicate . snd) list =
      Just $ Stack c (reverse before) after
  | otherwise = Nothing

findInColumns :: Window -> Columns -> Maybe (Stack (Rational, Column))
findInColumns window = findElement' (any ((== window) . snd))

findInColumn :: Window -> Column -> Maybe (Stack (Rational, Window))
findInColumn window = findElement' (== window)

removeWindows :: [Window] -> Columns -> Columns
removeWindows windows = removeEmptyColumns . fmap (second removeWindows')
  where
    inWindows (_, window) = window `notElem` windows
    removeWindows' = normalize . filter inWindows
    removeEmptyColumns = normalize . filter (not . null . snd)

removeWindow :: Window -> (Rational, Column) -> (Rational, Column)
removeWindow window = second (normalize . filter ((/= window) . snd))

addWindows :: [Window] -> Columns -> Columns
addWindows [] columns = columns
-- When there is only one column, create a new one on the right
addWindows windows [(_, windows')] = (1 % 2, windows') : singleColumn (1 % 2) 1 windows
-- When there is more, append the windows to the last column
addWindows windows columns
  | Just (columns', column) <- unsnoc columns =
      sanitizeColumns $ columns' <> [appendWindows windows column]
  | otherwise = singleColumn 1 1 windows

-- | Make sure the sum of all dimensions is 1
normalize :: [(Rational, a)] -> [(Rational, a)]
normalize elts = fmap (first (/ total)) elts
  where
    total = sum (fst <$> elts)

-- | Update the last dimension so that the sum of all dimensions is 1
sanitize :: [(Rational, a)] -> [(Rational, a)]
sanitize list
  | Just (elts, (_, a)) <- unsnoc list = elts <> [(1 - sum (fst <$> elts), a)]
  | otherwise = []

-- | Same on the whole layout
sanitizeColumns :: Columns -> Columns
sanitizeColumns = sanitize . fmap (second sanitize)

toOffsetRatio :: [(Rational, a)] -> [(Rational, Rational, a)]
toOffsetRatio ra = zipWith toTruple ra positions
  where
    toTruple (dimension, a) position = (position, dimension, a)
    positions = scanl' (\position (dimension, _) -> position + dimension) 0 ra

unsnoc :: [a] -> Maybe ([a], a)
unsnoc [] = Nothing
unsnoc (x : xs)
  | Just (is, l) <- unsnoc xs = Just (x : is, l)
  | otherwise = Just ([], x)
