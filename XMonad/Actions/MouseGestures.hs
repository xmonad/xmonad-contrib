-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Actions.MouseGestures
-- Description :  Support for simple mouse gestures.
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
    Direction2D(..),
    mouseGestureH,
    mouseGesture,
    mkCollect
) where

import XMonad.Prelude
import XMonad
import XMonad.Util.Types (Direction2D(..))

import Data.IORef
import qualified Data.Map as M
import Data.Map (Map)

-- $usage
--
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Actions.MouseGestures
-- > import qualified XMonad.StackSet as W
--
-- then add an appropriate mouse binding:
--
-- >     , ((modm .|. shiftMask, button3), mouseGesture gestures)
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

type Pos = (Position, Position)

delta :: Pos -> Pos -> Position
delta (ax, ay) (bx, by) = max (d ax bx) (d ay by)
    where
    d a b = abs (a - b)

dir :: Pos -> Pos -> Direction2D
dir (ax, ay) (bx, by) = trans . (/ pi) $ atan2 (fromIntegral $ ay - by) (fromIntegral $ bx - ax)
    where
    trans :: Double -> Direction2D
    trans x
        | rg (-3/4) (-1/4) x = D
        | rg (-1/4)  (1/4) x = R
        | rg  (1/4)  (3/4) x = U
        | otherwise          = L
    rg a z x = a <= x && x < z

gauge :: (Direction2D -> X ()) -> Pos -> IORef (Maybe (Direction2D, Pos)) -> Position -> Position -> X ()
gauge hook op st nx ny = do
    let np = (nx, ny)
    stx <- io $ readIORef st
    let pivot = maybe op snd stx
    when (significant np pivot) $ do
        let d' = dir pivot np
        when ((fst <$> stx) /= Just d') $ hook d'
        io $ writeIORef st (Just (d', np))
    where
    significant a b = delta a b >= 10

-- | @'mouseGestureH' moveHook endHook@ is a mouse button
-- event handler. It collects mouse movements, calling @moveHook@ for each
-- update; when the button is released, it calls @endHook@.
mouseGestureH :: (Direction2D -> X ()) -> X () -> X ()
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
mouseGesture :: Map [Direction2D] (Window -> X ()) -> Window -> X ()
mouseGesture tbl win = do
    (mov, end) <- mkCollect
    mouseGestureH (void . mov) $ end >>= \gest ->
        case M.lookup gest tbl of
            Nothing -> return ()
            Just f -> f win

-- | A callback generator for 'mouseGestureH'. 'mkCollect' returns two
-- callback functions for passing to 'mouseGestureH'. The move hook will
-- collect mouse movements (and return the current gesture as a list); the end
-- hook will return a list of the completed gesture, which you can access with
-- 'Control.Monad.>>='.
mkCollect :: (MonadIO m, MonadIO m') => m (Direction2D -> m' [Direction2D], m' [Direction2D])
mkCollect = liftIO $ do
    acc <- newIORef []
    let
        mov d = liftIO $ do
            ds <- readIORef acc
            let ds' = d : ds
            writeIORef acc ds'
            return $ reverse ds'
        end = liftIO $ do
            ds <- readIORef acc
            writeIORef acc []
            return $ reverse ds
    return (mov, end)
