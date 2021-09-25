{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.Drawer
-- Description :  A layout modifier to put windows in a "drawer".
-- Copyright   :  (c) 2009 Max Rabkin
-- License     :  BSD-style (see xmonad/LICENSE)
--
-- Maintainer  :  max.rabkin@gmail.com
-- Stability   :  unstable
-- Portability :  unportable
--
-- A layout modifier that puts some windows in a "drawer" which retracts and
-- expands depending on whether any window in it has focus.
--
-- Useful for music players, tool palettes, etc.
--
-----------------------------------------------------------------------------

module XMonad.Layout.Drawer
    ( -- * Usage
      -- $usage

      -- * Drawers
      simpleDrawer
    , drawer

      -- * Placing drawers
      -- The drawer can be placed on any side of the screen with these functions
    , onLeft, onTop, onRight, onBottom

    , module XMonad.Util.WindowProperties

    , Drawer, Reflected
    ) where

import XMonad
import XMonad.Layout.LayoutModifier
import XMonad.Util.WindowProperties
import XMonad.StackSet as S
import XMonad.Layout.Reflect

-- $usage
-- To use this module, add the following import to @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Layout.Drawer
--
-- > myLayout = drawer `onTop` (Tall 1 0.03 0.5) ||| Full ||| RandomOtherLayout...
-- >     where
-- >         drawer = simpleDrawer 0.01 0.3 (ClassName "Rhythmbox" `Or` ClassName "Xchat")
-- >
-- > main = xmonad def { layoutHook = myLayout }
--
-- This will place the Rhythmbox and Xchat windows in at the top of the screen
-- only when using the 'Tall' layout.  See "XMonad.Util.WindowProperties" for
-- more information on selecting windows.

data Drawer l a = Drawer Rational Rational Property (l a)
    deriving (Read, Show)

-- | filter : filterM :: partition : partitionM
partitionM :: Monad m => (a -> m Bool) -> [a] -> m ([a], [a])
partitionM _ [] = return ([], [])
partitionM f (x:xs) = do
    b <- f x
    (ys, zs) <- partitionM f xs
    return $ if b
                then (x:ys, zs)
                else (ys, x:zs)

instance (LayoutClass l Window, Read (l Window)) => LayoutModifier (Drawer l) Window where
    modifyLayout (Drawer rs rb p l) ws rect =
        case stack ws of
            Nothing -> runLayout ws rect
            Just stk@Stack{ up=up_, down=down_, S.focus=w } -> do
                    (upD, upM) <- partitionM (hasProperty p) up_
                    (downD, downM) <- partitionM (hasProperty p) down_
                    b <- hasProperty p w
                    focusedWindow <- gets (fmap S.focus . stack . workspace . current . windowset)

                    let rectD = if b && Just w == focusedWindow then rectB else rectS

                    let (stackD, stackM) = if b
                                            then ( Just $ stk { up=upD, down=downD }
                                                 , mkStack upM downM )
                                            else ( mkStack upD downD
                                                 , Just $ stk { up=upM, down=downM } )

                    (winsD, _) <- runLayout (ws { layout=l, stack=stackD }) rectD
                    (winsM, u') <- runLayout (ws { stack=stackM }) rectM
                    return (winsD ++ winsM, u')
      where
        mkStack [] [] = Nothing
        mkStack xs (y:ys) = Just (Stack { up=xs, S.focus=y, down=ys })
        mkStack (x:xs) ys = Just (Stack { up=xs, S.focus=x, down=ys })

        rectB = rect { rect_width=round $ fromIntegral (rect_width rect) * rb }
        rectS = rectB { rect_x=rect_x rectB - round ((rb - rs) * fromIntegral (rect_width rect)) }
        rectM = rect { rect_x=rect_x rect + round (fromIntegral (rect_width rect) * rs)
                     , rect_width=rect_width rect - round (fromIntegral (rect_width rect) * rs) }

type Reflected l = ModifiedLayout Reflect l

-- | Construct a drawer with a simple layout of the windows inside
simpleDrawer :: Rational -- ^ The portion of the screen taken up by the drawer when closed
              -> Rational   -- ^ The portion of the screen taken up by the drawer when open
              -> Property   -- ^ Which windows to put in the drawer
              -> Drawer Tall a
simpleDrawer rs rb p = Drawer rs rb p vertical
    where
        vertical = Tall 0 0 0

-- Export a synonym for the constructor as a Haddock workaround
-- | Construct a drawer with an arbitrary layout for windows inside
drawer ::    Rational   -- ^ The portion of the screen taken up by the drawer when closed
          -> Rational   -- ^ The portion of the screen taken up by the drawer when open
          -> Property   -- ^ Which windows to put in the drawer
          -> l a        -- ^ The layout of windows in the drawer
          -> Drawer l a
drawer = Drawer

onLeft :: Drawer l a -> l' a -> ModifiedLayout (Drawer l) l' a
onLeft = ModifiedLayout

onRight :: Drawer l a -> l' a -> Reflected (ModifiedLayout (Drawer l) (Reflected l')) a
onRight d = reflectHoriz . onLeft d . reflectHoriz

onTop :: Drawer l a -> l' a -> Mirror (ModifiedLayout (Drawer l) (Mirror l')) a
onTop d = Mirror . onLeft d . Mirror

onBottom :: Drawer l a -> l' a -> Reflected (Mirror (ModifiedLayout (Drawer l) (Mirror (Reflected l')))) a
onBottom d = reflectVert . onTop d . reflectVert
