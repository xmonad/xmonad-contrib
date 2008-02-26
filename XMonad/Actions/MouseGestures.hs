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
    mouseGesture,
    mouseGestureH
) where

import XMonad

import Data.IORef
import qualified Data.Map as M
import Data.Map (Map)
import Control.Monad

import System.IO

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

debugging :: Int
debugging = 0

collect :: ([Direction] -> X ()) -> IORef (Pos, [(Direction, Pos, Pos)]) -> Position -> Position -> X ()
collect hook st nx ny = do
    let np = (nx, ny)
    stx@(op, ds) <- io $ readIORef st
    let
        stx' =
            case ds of
            []
                | insignificant np op -> stx
                | otherwise -> (op, [(dir op np, np, op)])
            (d, zp, ap_) : ds'
                | insignificant np zp -> stx
                | otherwise ->
                    let
                        d' = dir zp np
                        ds''
                            | d == d'   = (d, np, ap_) : ds'
                            | otherwise = (d', np, zp) : ds
                    in (op, ds'')
    when (debugging > 0)
        . io . hPutStrLn stderr $ show "Mouse Gesture" ++ unwords (map show (extract stx')) ++ (if debugging > 1 then "; " ++ show op ++ "-" ++ show np else "")
    hook (extract stx')
    io $ writeIORef st stx'
    where
    insignificant a b = delta a b < 10

extract :: (Pos, [(Direction, Pos, Pos)]) -> [Direction]
extract (_, xs) = reverse . map (\(x, _, _) -> x) $ xs

-- | A utility function on top of 'mouseGestureH'. It uses a 'Data.Map.Map' to
-- look up the mouse gesture, then executes the corresponding action (if any).
mouseGesture :: Map [Direction] (Window -> X ()) -> Window -> X ()
mouseGesture tbl =
    mouseGestureH (const . const $ return ()) $ \win gest ->
        case M.lookup gest tbl of
            Nothing -> return ()
            Just f -> f win

-- | @'mouseGestureH' moveHook endHook gestures window@ is a mouse button
-- event handler. It collects mouse movements, calling @moveHook@ for each
-- update; when the button is released, it calls @endHook@ with the resulting
-- gesture.
mouseGestureH :: (Window -> [Direction] -> X ()) -> (Window -> [Direction] -> X ()) -> Window -> X ()
mouseGestureH moveHook endHook win = withDisplay $ \dpy -> do
    when (debugging > 1)
        . io . hPutStrLn stderr $ "mouseGesture " ++ show (win, dpy)
    root <- asks theRoot
    let win' = if win == none then root else win
    acc <- io $ do
        qp@(_, _, _, ix, iy, _, _, _) <- queryPointer dpy win'
        when (debugging > 1)
            . hPutStrLn stderr $ show "queryPointer" ++ show qp
        when (debugging > 1 && win' == none)
            . hPutStrLn stderr $ show "mouseGesture" ++ "zomg none"
        newIORef ((fromIntegral ix, fromIntegral iy), [])
    mouseDrag (collect (moveHook win') acc) $ do
        when (debugging > 0) . io . hPutStrLn stderr $ show ""
        gest <- io $ liftM extract $ readIORef acc
        endHook win' gest
