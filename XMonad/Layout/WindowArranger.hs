{-# LANGUAGE PatternGuards, FlexibleInstances, MultiParamTypeClasses #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.WindowArranger
-- Description :  A layout modifier to move and resize windows with the keyboard.
-- Copyright   :  (c) Andrea Rossato 2007
-- License     :  BSD-style (see xmonad/LICENSE)
--
-- Maintainer  :  andrea.rossato@unibz.it
-- Stability   :  unstable
-- Portability :  unportable
--
-- This is a pure layout modifier that will let you move and resize
-- windows with the keyboard in any layout.
-----------------------------------------------------------------------------

module XMonad.Layout.WindowArranger
    ( -- * Usage
      -- $usage
      windowArrange
    , windowArrangeAll
    , WindowArrangerMsg (..)
    , WindowArranger
    , memberFromList
    , listFromList
    , diff
    ) where

import XMonad
import XMonad.Prelude
import qualified XMonad.StackSet as S
import XMonad.Layout.LayoutModifier

import Control.Arrow ((***), (>>>), (&&&), first)

-- $usage
-- You can use this module with the following in your
-- @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Layout.WindowArranger
-- > myLayout = layoutHook def
-- > main = xmonad def { layoutHook = windowArrange myLayout }
--
-- or
--
-- > main = xmonad def { layoutHook = windowArrangeAll myLayout }
--
-- For more detailed instructions on editing the layoutHook see:
--
-- "XMonad.Doc.Extending#Editing_the_layout_hook"
--
-- You may also want to define some key binding to move or resize
-- windows. These are good defaults:
--
-- >        , ((modm .|. controlMask              , xK_s    ), sendMessage  Arrange         )
-- >        , ((modm .|. controlMask .|. shiftMask, xK_s    ), sendMessage  DeArrange       )
-- >        , ((modm .|. controlMask              , xK_Left ), sendMessage (MoveLeft      1))
-- >        , ((modm .|. controlMask              , xK_Right), sendMessage (MoveRight     1))
-- >        , ((modm .|. controlMask              , xK_Down ), sendMessage (MoveDown      1))
-- >        , ((modm .|. controlMask              , xK_Up   ), sendMessage (MoveUp        1))
-- >        , ((modm                 .|. shiftMask, xK_Left ), sendMessage (IncreaseLeft  1))
-- >        , ((modm                 .|. shiftMask, xK_Right), sendMessage (IncreaseRight 1))
-- >        , ((modm                 .|. shiftMask, xK_Down ), sendMessage (IncreaseDown  1))
-- >        , ((modm                 .|. shiftMask, xK_Up   ), sendMessage (IncreaseUp    1))
-- >        , ((modm .|. controlMask .|. shiftMask, xK_Left ), sendMessage (DecreaseLeft  1))
-- >        , ((modm .|. controlMask .|. shiftMask, xK_Right), sendMessage (DecreaseRight 1))
-- >        , ((modm .|. controlMask .|. shiftMask, xK_Down ), sendMessage (DecreaseDown  1))
-- >        , ((modm .|. controlMask .|. shiftMask, xK_Up   ), sendMessage (DecreaseUp    1))
--
-- For detailed instructions on editing your key bindings, see
-- "XMonad.Doc.Extending#Editing_key_bindings".

-- | A layout modifier to float the windows in a workspace
windowArrange :: l a -> ModifiedLayout WindowArranger l a
windowArrange = ModifiedLayout (WA True False [])

-- | A layout modifier to float all the windows in a workspace
windowArrangeAll :: l a -> ModifiedLayout WindowArranger l a
windowArrangeAll = ModifiedLayout (WA True True [])

data WindowArrangerMsg = DeArrange
                       | Arrange
                       | IncreaseLeft  Int
                       | IncreaseRight Int
                       | IncreaseUp    Int
                       | IncreaseDown  Int
                       | DecreaseLeft  Int
                       | DecreaseRight Int
                       | DecreaseUp    Int
                       | DecreaseDown  Int
                       | MoveLeft      Int
                       | MoveRight     Int
                       | MoveUp        Int
                       | MoveDown      Int
                       | SetGeometry   Rectangle
instance Message WindowArrangerMsg

data ArrangedWindow a = WR   (a, Rectangle)
                      | AWR  (a, Rectangle)
                        deriving (Read, Show)

type ArrangeAll = Bool
data WindowArranger a = WA Bool ArrangeAll [ArrangedWindow a] deriving (Read, Show)

instance (Show a, Read a, Eq a) => LayoutModifier WindowArranger a where
    pureModifier (WA True b   []) _ (Just _)               wrs = arrangeWindows b wrs

    pureModifier (WA True b awrs) _ (Just (S.Stack w _ _)) wrs = curry process wrs awrs
        where
          wins         = map fst       *** map awrWin
          update (a,r) = mkNewAWRs b a *** removeAWRs r >>> uncurry (++)
          process      = wins &&&  id  >>> first diff   >>> uncurry update >>>
                         replaceWR wrs >>> putOnTop w   >>> map fromAWR &&& Just . WA True b

    pureModifier _ _ _ wrs = (wrs, Nothing)

    pureMess (WA True b (wr:wrs)) m
        -- increase the window's size
        | Just (IncreaseRight i) <- fm, (win, Rectangle x y w h) <- fa = res win  x         y        (w + fi i) h
        | Just (IncreaseLeft  i) <- fm, (win, Rectangle x y w h) <- fa = res win (x - fi i) y        (w + fi i) h
        | Just (IncreaseUp    i) <- fm, (win, Rectangle x y w h) <- fa = res win  x        (y - fi i) w        (h + fi i)
        | Just (IncreaseDown  i) <- fm, (win, Rectangle x y w h) <- fa = res win  x         y         w        (h + fi i)
        -- decrease the window's size
        | Just (DecreaseRight i) <- fm, (win, Rectangle x y w h) <- fa = res win (x + fi i) y        (chk  w i) h
        | Just (DecreaseLeft  i) <- fm, (win, Rectangle x y w h) <- fa = res win  x         y        (chk  w i) h
        | Just (DecreaseUp    i) <- fm, (win, Rectangle x y w h) <- fa = res win  x         y         w        (chk h i)
        | Just (DecreaseDown  i) <- fm, (win, Rectangle x y w h) <- fa = res win  x        (y + fi i) w        (chk h i)
        --move the window around
        | Just (MoveRight     i) <- fm, (win, Rectangle x y w h) <- fa = res win (x + fi i) y         w         h
        | Just (MoveLeft      i) <- fm, (win, Rectangle x y w h) <- fa = res win (x - fi i) y         w         h
        | Just (MoveUp        i) <- fm, (win, Rectangle x y w h) <- fa = res win  x        (y - fi i) w         h
        | Just (MoveDown      i) <- fm, (win, Rectangle x y w h) <- fa = res win  x        (y + fi i) w         h

        where res wi x y w h = Just . WA True b $ AWR (wi,Rectangle x y w h):wrs
              fm             = fromMessage m
              fa             = fromAWR     wr
              chk        x y = fi $ max 1 (fi x - y)

    pureMess (WA t b (wr:wrs)) m
        | Just (SetGeometry   r) <- fromMessage m, (w,_) <- fromAWR wr = Just . WA t b $ AWR (w,r):wrs

    pureMess (WA _ b l) m
        | Just DeArrange <- fromMessage m = Just $ WA False b l
        | Just Arrange   <- fromMessage m = Just $ WA True  b l
        | otherwise                       = Nothing

arrangeWindows :: ArrangeAll -> [(a,Rectangle)] -> ([(a, Rectangle)], Maybe (WindowArranger a))
arrangeWindows b wrs = (wrs, Just $ WA True b (map t wrs))
    where t = if b then AWR else WR

fromAWR :: ArrangedWindow a -> (a, Rectangle)
fromAWR (WR   x) = x
fromAWR (AWR  x) = x

awrWin :: ArrangedWindow a -> a
awrWin = fst . fromAWR

getAWR :: Eq a => a -> [ArrangedWindow a] -> [ArrangedWindow a]
getAWR = memberFromList awrWin (==)

getWR ::  Eq a => a -> [(a,Rectangle)] -> [(a,Rectangle)]
getWR = memberFromList fst (==)

mkNewAWRs :: Eq a => ArrangeAll -> [a] -> [(a,Rectangle)] -> [ArrangedWindow a]
mkNewAWRs b w wrs = map t . concatMap (`getWR` wrs) $ w
    where t = if b then AWR else WR

removeAWRs :: Eq a => [a] -> [ArrangedWindow a] -> [ArrangedWindow a]
removeAWRs = listFromList awrWin notElem

putOnTop :: Eq a => a -> [ArrangedWindow a] -> [ArrangedWindow a]
putOnTop w awrs = awr ++ nawrs
    where awr   = getAWR w awrs
          nawrs = filter ((/=w) . awrWin) awrs

replaceWR :: Eq a => [(a, Rectangle)] -> [ArrangedWindow a] -> [ArrangedWindow a]
replaceWR wrs = foldr r []
    where r x xs
              | WR wr <- x = case fst wr `elemIndex` map fst wrs of
                               Just i  -> WR (wrs !! i):xs
                               Nothing -> x:xs
              | otherwise  = x:xs

-- | Given a function to be applied to each member of a list, and a
-- function to check a condition by processing this transformed member
-- with the members of a list, you get the list of members that
-- satisfy the condition.
listFromList :: (b -> c) -> (c -> [a] -> Bool) -> [a] -> [b] -> [b]
listFromList f g l = foldr (h l) []
    where h x y ys = if g (f y) x then y:ys else ys

-- | Given a function to be applied to each member of ta list, and a
-- function to check a condition by processing this transformed member
-- with something, you get the first member that satisfy the condition,
-- or an empty list.
memberFromList :: (b -> c) -> (c -> a -> Bool) -> a -> [b] -> [b]
memberFromList f g l = foldr (h l) []
    where h x y ys = if g (f y) x then [y] else ys

-- | Get the list of elements to be deleted and the list of elements to
-- be added to the first list in order to get the second list.
diff :: Eq a => ([a],[a]) -> ([a],[a])
diff (x,y) = (x \\ y, y \\ x)
