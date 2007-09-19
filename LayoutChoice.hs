{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  XMonadContrib.LayoutChoice
-- Copyright   :  (c) David Roundy
-- License     :  BSD-style (see xmonad/LICENSE)
-- 
-- Maintainer  :  email@address.com
-- Stability   :  unstable
-- Portability :  unportable
--
-- A tabbed layout for the Xmonad Window Manager 
--
-----------------------------------------------------------------------------

module XMonadContrib.LayoutChoice ( 
                             -- * Usage:
                             -- $usage
                              layoutChoice
                            , ChangeLayout(..)
                            ) where

import Data.List ( partition )
import Data.Maybe ( fromMaybe )
import XMonad
import Operations ( tall, UnDoLayout(..) )

-- $usage
-- You can use this module to replace the default layout handling of
-- xmonad.  See the docstring docs for example usage.

-- %import XMonadContrib.LayoutChoice
-- %layout , layoutChoice [("full", full),
-- %layout                 ("tall", tall 1 0.03 0.5)]

-- %keybind , ((modMask, xK_space), sendMessage NextLayout)
-- %keybind , ((modMask .|. shiftMask, xK_space), sendMessage PrevLayout)
-- %keybind , ((modMask, xK_f), sendMessage (JumpToLayout "full"))

data ChangeLayout = NextLayout | PrevLayout | JumpToLayout String
                 deriving ( Eq, Show, Typeable )
instance Message ChangeLayout

layoutChoice :: [(String, Layout a)] -> Layout a
layoutChoice [] = tall 1 0.03 0.5
layoutChoice ((n,l):ls) = Layout { doLayout = dolay
                                 , modifyLayout = md }
    where dolay r s = do (x,ml') <- doLayout l r s
                         return (x, (\l' -> layoutChoice ((n,l'):ls)) `fmap` ml')
          md m | Just NextLayout <- fromMessage m = switchl rls
               | Just PrevLayout <- fromMessage m = switchl rls'
               | Just (JumpToLayout x) <- fromMessage m = switchl (j x)
               | otherwise = do ml' <- modifyLayout l m
                                return $ (\l' -> layoutChoice ((n,l'):ls)) `fmap` ml'
          
          rls (x:xs) = xs ++ [x]
          rls [] = []
          rls' = reverse . rls . reverse
          j s zs = case partition (\z -> s == fst z) zs of
                   (xs,ys) -> xs++ys
          switchl f = do ml' <- modifyLayout l (SomeMessage UnDoLayout)
                         return $ Just (layoutChoice $ f $ (n,fromMaybe l ml'):ls)
