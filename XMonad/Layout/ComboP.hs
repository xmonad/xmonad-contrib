{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, PatternGuards #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.ComboP
-- Description :  Combine multiple layouts and specify where to put new windows.
-- Copyright   :  (c) Konstantin Sobolev <konstantin.sobolev@gmail.com>
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Konstantin Sobolev <konstantin.sobolev@gmail.com>
-- Stability   :  unstable
-- Portability :  unportable
--
-- A layout that combines multiple layouts and allows to specify where to put
-- new windows.
--
-----------------------------------------------------------------------------

module XMonad.Layout.ComboP (
                             -- * Usage
                             -- $usage
                             combineTwoP,
                             CombineTwoP,
                             SwapWindow(..),
                             PartitionWins(..),
                             Property(..)
                            ) where

import XMonad.Prelude
import XMonad hiding (focus)
import XMonad.StackSet ( Workspace (..), Stack(..) )
import XMonad.Layout.WindowNavigation
import XMonad.Util.WindowProperties
import qualified XMonad.StackSet as W

-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Layout.ComboP
--
-- and add something like
--
-- > combineTwoP (TwoPane 0.03 0.5) (tabbed shrinkText defaultTConf) (tabbed shrinkText defaultTConf) (ClassName "Firefox")
--
-- to your layouts. This way all windows with class = \"Firefox\" will always go
-- to the left pane, all others - to the right.
--
-- For more detailed instructions on editing the layoutHook see:
--
-- "XMonad.Doc.Extending#Editing_the_layout_hook"
--
-- 'combineTwoP' is a simple layout combinator based on 'combineTwo' from Combo, with
-- addition of a 'Property' which tells where to put new windows. Windows mathing
-- the property will go into the first part, all others will go into the second
-- part. It supports @Move@ messages as 'combineTwo' does, but it also introduces
-- 'SwapWindow' message which sends focused window to the other part. It is
-- required because @Move@ commands don't work when one of the parts is empty.
-- To use it, import \"XMonad.Layout.WindowNavigation\", and add the following key
-- bindings (or something similar):
--
-- >    , ((modm .|. controlMask .|. shiftMask, xK_Right), sendMessage $ Move R)
-- >    , ((modm .|. controlMask .|. shiftMask, xK_Left ), sendMessage $ Move L)
-- >    , ((modm .|. controlMask .|. shiftMask, xK_Up   ), sendMessage $ Move U)
-- >    , ((modm .|. controlMask .|. shiftMask, xK_Down ), sendMessage $ Move D)
-- >    , ((modm .|. controlMask .|. shiftMask, xK_s    ), sendMessage $ SwapWindow)
--
-- For detailed instruction on editing the key binding see
-- "XMonad.Doc.Extending#Editing_key_bindings".

data SwapWindow =  SwapWindow        -- ^ Swap window between panes
                 | SwapWindowN Int   -- ^ Swap window between panes in the N-th nested ComboP. @SwapWindowN 0@ equals to SwapWindow
                 deriving (Read, Show)
instance Message SwapWindow

data PartitionWins = PartitionWins  -- ^ Reset the layout and
                                    -- partition all windows into the
                                    -- correct sub-layout.  Useful for
                                    -- when window properties have
                                    -- changed and you want ComboP to
                                    -- update which layout a window
                                    -- belongs to.
                   deriving (Read, Show)
instance Message PartitionWins

data CombineTwoP l l1 l2 a = C2P [a] [a] [a] l (l1 a) (l2 a) Property
                                deriving (Read, Show)

combineTwoP :: (LayoutClass super(), LayoutClass l1 Window, LayoutClass l2 Window) =>
                super () -> l1 Window -> l2 Window -> Property -> CombineTwoP (super ()) l1 l2 Window
combineTwoP = C2P [] [] []

instance (LayoutClass l (), LayoutClass l1 Window, LayoutClass l2 Window) =>
    LayoutClass (CombineTwoP (l ()) l1 l2) Window where
    doLayout (C2P f w1 w2 super l1 l2 prop) rinput s =
        let origws = W.integrate s           -- passed in windows
            w1c = origws `intersect` w1      -- current windows in the first pane
            w2c = origws `intersect` w2      -- current windows in the second pane
            new = origws \\ (w1c ++ w2c)     -- new windows
            superstack = Just Stack { focus=(), up=[], down=[()] }
            f' = focus s:delete (focus s) f  -- list of focused windows, contains 2 elements at most
        in do
            matching <- hasProperty prop `filterM` new  -- new windows matching predecate
            let w1' = w1c ++ matching                     -- updated first pane windows
                w2' = w2c ++ (new \\ matching)            -- updated second pane windows
                s1 = differentiate f' w1'                 -- first pane stack
                s2 = differentiate f' w2'                 -- second pane stack
            ([((),r1),((),r2)], msuper') <- runLayout (Workspace "" super superstack) rinput
            (wrs1, ml1') <- runLayout (Workspace "" l1 s1) r1
            (wrs2, ml2') <- runLayout (Workspace "" l2 s2) r2
            return  (wrs1++wrs2, Just $ C2P f' w1' w2' (fromMaybe super msuper')
                (fromMaybe l1 ml1') (fromMaybe l2 ml2') prop)

    handleMessage us@(C2P f ws1 ws2 super l1 l2 prop) m
        | Just PartitionWins   <- fromMessage m = return . Just $ C2P [] [] [] super l1 l2 prop
        | Just SwapWindow      <- fromMessage m = swap us
        | Just (SwapWindowN 0) <- fromMessage m = swap us
        | Just (SwapWindowN n) <- fromMessage m = forwardToFocused us $ SomeMessage $ SwapWindowN $ n-1

        | Just (MoveWindowToWindow w1 w2) <- fromMessage m,
          w1 `elem` ws1,
          w2 `elem` ws2 = return $ Just $ C2P f (delete w1 ws1) (w1:ws2) super l1 l2 prop

        | Just (MoveWindowToWindow w1 w2) <- fromMessage m,
          w1 `elem` ws2,
          w2 `elem` ws1 = return $ Just $ C2P f (w1:ws1) (delete w1 ws2) super l1 l2 prop

        | otherwise = do ml1' <- handleMessage l1 m
                         ml2' <- handleMessage l2 m
                         msuper' <- handleMessage super m
                         if isJust msuper' || isJust ml1' || isJust ml2'
                            then return $ Just $ C2P f ws1 ws2
                                                 (fromMaybe super msuper')
                                                 (fromMaybe l1 ml1')
                                                 (fromMaybe l2 ml2') prop
                            else return Nothing

    description (C2P _ _ _ super l1 l2 prop) = "combining " ++ description l1 ++ " and "++
                                description l2 ++ " with " ++ description super ++ " using "++ show prop

-- send focused window to the other pane. Does nothing if we don't
-- own the focused window
swap :: (LayoutClass s a, LayoutClass l1 Window, LayoutClass l2 Window) =>
        CombineTwoP (s a) l1 l2 Window -> X (Maybe (CombineTwoP (s a) l1 l2 Window))
swap (C2P f ws1 ws2 super l1 l2 prop) = do
    mst <- gets (W.stack . W.workspace . W.current . windowset)
    let (ws1', ws2') = case mst of
            Nothing -> (ws1, ws2)
            Just st -> if foc `elem` ws1
                           then (foc `delete` ws1, foc:ws2)
                           else if foc `elem` ws2
                               then (foc:ws1, foc `delete` ws2)
                               else (ws1, ws2)
                       where foc = W.focus st
    if (ws1,ws2) == (ws1',ws2')
        then return Nothing
        else return $ Just $ C2P f ws1' ws2' super l1 l2 prop


-- forwards the message to the sublayout which contains the focused window
forwardToFocused :: (LayoutClass l1 Window, LayoutClass l2 Window, LayoutClass s a) =>
                    CombineTwoP (s a) l1 l2 Window -> SomeMessage -> X (Maybe (CombineTwoP (s a) l1 l2 Window))
forwardToFocused (C2P f ws1 ws2 super l1 l2 prop) m = do
    ml1 <- forwardIfFocused l1 ws1 m
    ml2 <- forwardIfFocused l2 ws2 m
    ms <- if isJust ml1 || isJust ml2
            then return Nothing
            else handleMessage super m
    if isJust ml1 || isJust ml2 || isJust ms
        then return $ Just $ C2P f ws1 ws2 (fromMaybe super ms) (fromMaybe l1 ml1) (fromMaybe l2 ml2) prop
        else return Nothing

-- forwards message m to layout l if focused window is among w
forwardIfFocused :: (LayoutClass l Window) => l Window -> [Window] -> SomeMessage -> X (Maybe (l Window))
forwardIfFocused l w m = do
    mst <- gets (W.stack . W.workspace . W.current . windowset)
    maybe (return Nothing) send mst where
    send st = if W.focus st `elem` w
                then handleMessage l m
                else return Nothing

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

-- vim:ts=4:shiftwidth=4:softtabstop=4:expandtab:foldlevel=20:
