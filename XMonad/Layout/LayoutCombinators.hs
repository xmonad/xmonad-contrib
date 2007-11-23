{-# OPTIONS_GHC -fglasgow-exts #-} -- For deriving Data/Typeable
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, PatternGuards #-}
-----------------------------------------------------------------------------
-- |
-- Module       : XMonad.Layout.LayoutCombinators
-- Copyright    : (c) David Roundy <droundy@darcs.net>
-- License      : BSD
--
-- Maintainer   : David Roundy <droundy@darcs.net>
-- Stability    : unstable
-- Portability  : portable
--
-- A module for combining other layouts.
-----------------------------------------------------------------------------

module XMonad.Layout.LayoutCombinators (
    -- * Usage
    -- $usage
    (<||>),(<-||>),(<||->),
    (<//>),(<-//>),(<//->),
    (<|>),(<-|>),(<|->),
    (</>),(<-/>),(</->),
    (|||),
    JumpToLayout(JumpToLayout)
    ) where

import Data.Maybe ( isJust, isNothing )

import XMonad
import XMonad.Layouts ( Tall(..), Mirror(..), ChangeLayout(NextLayout) )
import XMonad.Layout.Combo
import XMonad.Layout.DragPane

-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Layout.LayoutCombinators hiding ( (|||) )
--
-- Then edit your @layoutHook@ by using the new layout combinators:
--
-- > myLayouts = (Tall 1 (3/100) (1/2) <-/> Full)  ||| (Tall 1 (3/100) (1/2) <||-> Full) ||| Full ||| etc..
-- > main = xmonad defaultConfig { layoutHook = myLayouts }
--
-- For more detailed instructions on editing the layoutHook see:
--
-- "XMonad.Doc.Extending#Editing_the_layout_hook"

infixr 6 <||>, <//>, <-||>, <-//>, <||->, <//->, <|>, <-|>, <|->, </>, <-/>, </->

-- | Combines two layouts vertically using dragPane
(<||>) :: (Read a, Eq a, LayoutClass l1 a, LayoutClass l2 a) =>
          l1 a -> l2 a -> CombineTwo (DragPane ()) l1 l2 a

-- | Combines two layouts vertically using dragPane giving more screen
-- to the first layout
(<-||>) :: (Read a, Eq a, LayoutClass l1 a, LayoutClass l2 a) =>
          l1 a -> l2 a -> CombineTwo (DragPane ()) l1 l2 a

-- | Combines two layouts vertically using dragPane giving more screen
-- to the second layout
(<||->) :: (Read a, Eq a, LayoutClass l1 a, LayoutClass l2 a) =>
          l1 a -> l2 a -> CombineTwo (DragPane ()) l1 l2 a

-- | Combines two layouts horizzontally using dragPane
(<//>) :: (Read a, Eq a, LayoutClass l1 a, LayoutClass l2 a) =>
          l1 a -> l2 a -> CombineTwo (DragPane ()) l1 l2 a

-- | Combines two layouts horizzontally using dragPane giving more screen
-- to the first layout
(<-//>) :: (Read a, Eq a, LayoutClass l1 a, LayoutClass l2 a) =>
          l1 a -> l2 a -> CombineTwo (DragPane ()) l1 l2 a

-- | Combines two layouts horizzontally using dragPane giving more screen
-- to the first layout
(<//->) :: (Read a, Eq a, LayoutClass l1 a, LayoutClass l2 a) =>
          l1 a -> l2 a -> CombineTwo (DragPane ()) l1 l2 a

-- | Combines two layouts vertically using Tall
(<|>) :: (Read a, Eq a, LayoutClass l1 a, LayoutClass l2 a)
          => l1 a -> l2 a -> CombineTwo (Tall ()) l1 l2 a

-- | Combines two layouts vertically using Tall giving more screen
-- to the first layout
(<-|>) :: (Read a, Eq a, LayoutClass l1 a, LayoutClass l2 a)
          => l1 a -> l2 a -> CombineTwo (Tall ()) l1 l2 a

-- | Combines two layouts vertically using Tall giving more screen
-- to the second layout
(<|->) :: (Read a, Eq a, LayoutClass l1 a, LayoutClass l2 a)
          => l1 a -> l2 a -> CombineTwo (Tall ()) l1 l2 a

-- | Combines two layouts horizzontally using Mirror Tall (a wide
-- layout)
(</>) :: (Read a, Eq a, LayoutClass l1 a, LayoutClass l2 a)
          => l1 a -> l2 a -> CombineTwo (Mirror Tall ()) l1 l2 a

-- | Combines two layouts horizzontally using Mirror Tall (a wide
-- layout) giving more screen to the first layout
(<-/>) :: (Read a, Eq a, LayoutClass l1 a, LayoutClass l2 a)
          => l1 a -> l2 a -> CombineTwo (Mirror Tall ()) l1 l2 a

-- | Combines two layouts horizzontally using Mirror Tall (a wide
-- layout) giving more screen to the second layout
(</->) :: (Read a, Eq a, LayoutClass l1 a, LayoutClass l2 a)
          => l1 a -> l2 a -> CombineTwo (Mirror Tall ()) l1 l2 a

-- implementation
(<||>)  = combineTwo (dragPane Vertical 0.1 0.5)
(<-||>) = combineTwo (dragPane Vertical 0.1 0.2)
(<||->) = combineTwo (dragPane Vertical 0.1 0.8)
(<//>)  = combineTwo (dragPane Horizontal 0.1 0.5)
(<-//>) = combineTwo (dragPane Horizontal 0.1 0.8)
(<//->) = combineTwo (dragPane Horizontal 0.1 0.2)
(<|>)   = combineTwo (Tall 1 0.1 0.5)
(<-|>)  = combineTwo (Tall 1 0.1 0.8)
(<|->)  = combineTwo (Tall 1 0.1 0.1)
(</>)   = combineTwo (Mirror $ Tall 1 0.1 0.5)
(<-/>)  = combineTwo (Mirror $ Tall 1 0.1 0.8)
(</->)  = combineTwo (Mirror $ Tall 1 0.1 0.2)

infixr 5 |||

-- | A new layout combinator that allows the use of a prompt to change
-- layout. For more information see "Xmonad.Prompt.Layout"
(|||) :: (LayoutClass l1 a, LayoutClass l2 a) => l1 a -> l2 a -> NewSelect l1 l2 a
(|||) = NewSelect True

data NewSelect l1 l2 a = NewSelect Bool (l1 a) (l2 a) deriving ( Read, Show )

data NoWrap = NextLayoutNoWrap | Wrap deriving ( Read, Show, Typeable )
instance Message NoWrap

data JumpToLayout = JumpToLayout String deriving ( Read, Show, Typeable )
instance Message JumpToLayout

instance (LayoutClass l1 a, LayoutClass l2 a) => LayoutClass (NewSelect l1 l2) a where
    doLayout (NewSelect True l1 l2) r s = do (wrs, ml1') <- doLayout l1 r s
                                             return (wrs, (\l1' -> NewSelect True l1' l2) `fmap` ml1')
    doLayout (NewSelect False l1 l2) r s = do (wrs, ml2') <- doLayout l2 r s
                                              return (wrs, (\l2' -> NewSelect False l1 l2') `fmap` ml2')
    description (NewSelect True l1 _) = description l1
    description (NewSelect False _ l2) = description l2
    handleMessage l@(NewSelect False _ _) m
        | Just Wrap <- fromMessage m = fmap Just $ swap l >>= passOn m
    handleMessage l@(NewSelect amfirst _ _) m
        | Just NextLayoutNoWrap <- fromMessage m =
                  if amfirst then when' isNothing (passOnM m l) $
                                  fmap Just $ swap l >>= passOn (SomeMessage Wrap)
                             else passOnM m l
    handleMessage l m
        | Just NextLayout <- fromMessage m = when' isNothing (passOnM (SomeMessage NextLayoutNoWrap) l) $
                                             fmap Just $ swap l >>= passOn (SomeMessage Wrap)
    handleMessage l@(NewSelect True _ l2) m
        | Just (JumpToLayout d) <- fromMessage m, d == description l2 = Just `fmap` swap l
    handleMessage l@(NewSelect False l1 _) m
        | Just (JumpToLayout d) <- fromMessage m, d == description l1 = Just `fmap` swap l
    handleMessage l m
        | Just (JumpToLayout _) <- fromMessage m = when' isNothing (passOnM m l) $
                                                   do ml' <- passOnM m $ sw l
                                                      case ml' of
                                                        Nothing -> return Nothing
                                                        Just l' -> Just `fmap` swap (sw l')
    handleMessage (NewSelect b l1 l2) m
        | Just ReleaseResources  <- fromMessage m =
        do ml1' <- handleMessage l1 m
           ml2' <- handleMessage l2 m
           return $ if isJust ml1' || isJust ml2'
                    then Just $ NewSelect b (maybe l1 id ml1') (maybe l2 id ml2')
                    else Nothing
    handleMessage l m = passOnM m l

swap :: (LayoutClass l1 a, LayoutClass l2 a) => NewSelect l1 l2 a -> X (NewSelect l1 l2 a)
swap l = sw `fmap` passOn (SomeMessage Hide) l

sw :: NewSelect l1 l2 a -> NewSelect l1 l2 a
sw (NewSelect b lt lf) = NewSelect (not b) lt lf

passOn :: (LayoutClass l1 a, LayoutClass l2 a) =>
          SomeMessage -> NewSelect l1 l2 a -> X (NewSelect l1 l2 a)
passOn m l = maybe l id `fmap` passOnM m l

passOnM :: (LayoutClass l1 a, LayoutClass l2 a) =>
           SomeMessage -> NewSelect l1 l2 a -> X (Maybe (NewSelect l1 l2 a))
passOnM m (NewSelect True lt lf) = do mlt' <- handleMessage lt m
                                      return $ (\lt' -> NewSelect True lt' lf) `fmap` mlt'
passOnM m (NewSelect False lt lf) = do mlf' <- handleMessage lf m
                                       return $ (\lf' -> NewSelect False lt lf') `fmap` mlf'

when' :: Monad m => (a -> Bool) -> m a -> m a -> m a
when' f a b = do a1 <- a; if f a1 then b else return a1

--  LocalWords:  horizzontally
