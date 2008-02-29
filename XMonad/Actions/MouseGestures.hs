-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Actions.MouseGestures
-- Copyright   :  (c) Lukas Mai
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  <l.mai@web.de>
-- Stability   :  unstable
-- Portability :  unportable
--
-- Support for simple mouse gestures.
--
-----------------------------------------------------------------------------

module XMonad.Actions.MouseGestures (
    -- * Usage
    -- $usage
    Direction(..),
    mouseGestureH,
    mouseGesture,
    mkCollect
) where

import XMonad

import Data.IORef
import qualified Data.Map as M
import Data.Map (Map)
import Data.Maybe
import Control.Monad

-- $usage
--
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Actions.MouseGestures
-- > import qualified XMonad.StackSet as W
--
-- then add an appropriate mouse binding:
--
-- >     , ((modMask x .|. shiftMask, button3), mouseGesture gestures)
--
-- where @gestures@ is a 'Data.Map.Map' from gestures to actions on
-- windows, for example:
--
-- >     gestures = M.fromList
-- >         [ ([], focus)
-- >         , ([U], \w -> focus w >> windows W.swapUp)
-- >         , ([D], \w -> focus w >> windows W.swapDown)
-- >         , ([R, D], \_ -> sendMessage NextLayout)
-- >         ]
--
-- This is just an example, of course; you can use any mouse button and
-- gesture definitions you want.
--
-- For detailed instructions on editing your mouse bindings, see
-- "XMonad.Doc.Extending#Editing_mouse_bindings".

-- | The four cardinal screen directions. A \"gesture\" is a sequence of
--   directions.
data Direction = L | U | R | D
    deriving (Eq, Ord, Show, Read, Enum, Bounded)

type Pos = (Position, Position)

delta :: Pos -> Pos -> Position
delta (ax, ay) (bx, by) = max (d ax bx) (d ay by)
    where
    d a b = abs (a - b)

dir :: Pos -> Pos -> Direction
dir (ax, ay) (bx, by) = trans . (/ pi) $ atan2 (fromIntegral $ ay - by) (fromIntegral $ bx - ax)
    where
    trans :: Double -> Direction
    trans x
        | rg (-3/4) (-1/4) x = D
        | rg (-1/4)  (1/4) x = R
        | rg  (1/4)  (3/4) x = U
        | otherwise          = L
    rg a z x = a <= x && x < z

gauge :: (Direction -> X ()) -> Pos -> IORef (Maybe (Direction, Pos)) -> Position -> Position -> X ()
gauge hook op st nx ny = do
    let np = (nx, ny)
    stx <- io $ readIORef st
    let
        (~(Just od), pivot) = case stx of
            Nothing -> (Nothing, op)
            Just (d, zp) -> (Just d, zp)
        cont = do
            guard $ significant np pivot
            return $ do
                let d' = dir pivot np
                when (isNothing stx || od /= d') $ hook d'
                io $ writeIORef st (Just (d', np))
    fromMaybe (return ()) cont
    where
    significant a b = delta a b >= 10

-- | @'mouseGestureH' moveHook endHook@ is a mouse button
-- event handler. It collects mouse movements, calling @moveHook@ for each
-- update; when the button is released, it calls @endHook@.
mouseGestureH :: (Direction -> X ()) -> X () -> X ()
mouseGestureH moveHook endHook = do
    dpy <- asks display
    root <- asks theRoot
    (pos, acc) <- io $ do
        (_, _, _, ix, iy, _, _, _) <- queryPointer dpy root
        r <- newIORef Nothing
        return ((fromIntegral ix, fromIntegral iy), r)
    mouseDrag (gauge moveHook pos acc) endHook

-- | A utility function on top of 'mouseGestureH'. It uses a 'Data.Map.Map' to
-- look up the mouse gesture, then executes the corresponding action (if any).
mouseGesture :: Map [Direction] (Window -> X ()) -> Window -> X ()
mouseGesture tbl win = do
    (mov, end) <- mkCollect
    mouseGestureH (\d -> mov d >> return ()) $ end >>= \gest ->
        case M.lookup gest tbl of
            Nothing -> return ()
            Just f -> f win

-- | A callback generator for 'mouseGestureH'. 'mkCollect' returns two
-- callback functions for passing to 'mouseGestureH'. The move hook will
-- collect mouse movements (and return the current gesture as a list); the end
-- hook will return a list of the completed gesture, which you can access with
-- 'Control.Monad.>>='.
mkCollect :: (MonadIO m) => m (Direction -> X [Direction], X [Direction])
mkCollect = liftIO $ do
    acc <- newIORef []
    let
        mov d = io $ do
            ds <- readIORef acc
            let ds' = d : ds
            writeIORef acc ds'
            return $ reverse ds'
        end = io $ do
            ds <- readIORef acc
            writeIORef acc []
            return $ reverse ds
    return (mov, end)
