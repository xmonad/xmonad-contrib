-----------------------------------------------------------------------------
-- |
-- Module       : XMonad.Layout.LayoutCombinators
-- Description :  Easily combine multiple layouts into one composite layout.
-- Copyright    : (c) David Roundy <droundy@darcs.net>
-- License      : BSD
--
-- Maintainer   : none
-- Stability    : unstable
-- Portability  : portable
--
-- The "XMonad.Layout.LayoutCombinators" module provides combinators
-- for easily combining multiple layouts into one composite layout.
-----------------------------------------------------------------------------

module XMonad.Layout.LayoutCombinators
    ( -- * Usage
      -- $usage

      -- * Layout combinators
      -- $combine

      -- ** Combinators using DragPane vertical
      -- $dpv
      (*||*), (**||*),(***||*),(****||*),(***||**),(****||***)
    , (***||****),(*||****),(**||***),(*||***),(*||**)

      -- ** Combinators using DragPane horizontal
      -- $dph
    , (*//*), (**//*),(***//*),(****//*),(***//**),(****//***)
    , (***//****),(*//****),(**//***),(*//***),(*//**)

      -- ** Combinators using Tall (vertical)
      -- $tv
    , (*|*), (**|*),(***|*),(****|*),(***|**),(****|***)
    , (***|****),(*|****),(**|***),(*|***),(*|**)

      -- ** Combinators using Mirror Tall (horizontal)
      -- $mth
    , (*/*), (**/*),(***/*),(****/*),(***/**),(****/***)
    , (***/****),(*/****),(**/***),(*/***),(*/**)

      -- * Re-exports for backwards compatibility
    , (|||)
    , JumpToLayout(..)
    , NewSelect
    ) where

import XMonad
import XMonad.Layout.Combo
import XMonad.Layout.DragPane

-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Layout.LayoutCombinators
--
-- Then edit your @layoutHook@ to use the new layout combinators. For
-- example:
--
-- > myLayout = (Tall 1 (3/100) (1/2) *//* Full)  ||| (Tall 1 (3/100) (1/2) ***||** Full) ||| Full ||| etc..
-- > main = xmonad def { layoutHook = myLayout }
--
-- For more detailed instructions on editing the @layoutHook@ see:
--
-- "XMonad.Doc.Extending#Editing_the_layout_hook"
--

-- $combine
-- Each of the following combinators combines two layouts into a
-- single composite layout by splitting the screen into two regions,
-- one governed by each layout.  Asterisks in the combinator names
-- denote the relative amount of screen space given to the respective
-- layouts.  For example, the '***||*' combinator gives three times as
-- much space to the left-hand layout as to the right-hand layout.

infixr 6 *||*, **||*, ***||*, ****||*, ***||**, ****||***, ***||****, *||****, **||***, *||***, *||**,
         *//*, **//*, ***//*, ****//*, ***//**, ****//***, ***//****, *//****, **//***, *//***, *//**,
         *|* , **|* , ***|* , ****|* , ***|** , ****|*** , ***|**** , *|**** , **|*** , *|*** , *|** ,
         */* , **/* , ***/* , ****/* , ***/** , ****/*** , ***/**** , */**** , **/*** , */*** , */**

-- $dpv
-- These combinators combine two layouts using "XMonad.DragPane" in
-- vertical mode.

(*||*),(**||*),(***||*),(****||*), (***||**),(****||***),
       (***||****),(*||****),(**||***),(*||***),(*||**) :: (Read a, Eq a, LayoutClass l1 a, LayoutClass l2 a) =>
          l1 a -> l2 a -> CombineTwo (DragPane ()) l1 l2 a

(*||*)      = combineTwo (dragPane Vertical 0.1 (1/2))
(**||*)     = combineTwo (dragPane Vertical 0.1 (2/3))
(***||*)    = combineTwo (dragPane Vertical 0.1 (3/4))
(****||*)   = combineTwo (dragPane Vertical 0.1 (4/5))
(***||**)   = combineTwo (dragPane Vertical 0.1 (3/5))
(****||***) = combineTwo (dragPane Vertical 0.1 (4/7))
(***||****) = combineTwo (dragPane Vertical 0.1 (3/7))
(*||****)   = combineTwo (dragPane Vertical 0.1 (1/5))
(**||***)   = combineTwo (dragPane Vertical 0.1 (2/5))
(*||***)    = combineTwo (dragPane Vertical 0.1 (1/4))
(*||**)     = combineTwo (dragPane Vertical 0.1 (1/3))

-- $dph
-- These combinators combine two layouts using "XMonad.DragPane" in
-- horizontal mode.

(*//*),(**//*),(***//*),(****//*), (***//**),(****//***),
       (***//****),(*//****),(**//***),(*//***),(*//**) :: (Read a, Eq a, LayoutClass l1 a, LayoutClass l2 a) =>
          l1 a -> l2 a -> CombineTwo (DragPane ()) l1 l2 a

(*//*)      = combineTwo (dragPane Horizontal 0.1 (1/2))
(**//*)     = combineTwo (dragPane Horizontal 0.1 (2/3))
(***//*)    = combineTwo (dragPane Horizontal 0.1 (3/4))
(****//*)   = combineTwo (dragPane Horizontal 0.1 (4/5))
(***//**)   = combineTwo (dragPane Horizontal 0.1 (3/5))
(****//***) = combineTwo (dragPane Horizontal 0.1 (4/7))
(***//****) = combineTwo (dragPane Horizontal 0.1 (3/7))
(*//****)   = combineTwo (dragPane Horizontal 0.1 (1/5))
(**//***)   = combineTwo (dragPane Horizontal 0.1 (2/5))
(*//***)    = combineTwo (dragPane Horizontal 0.1 (1/4))
(*//**)     = combineTwo (dragPane Horizontal 0.1 (1/3))

-- $tv
-- These combinators combine two layouts vertically using @Tall@.

(*|*),(**|*),(***|*),(****|*), (***|**),(****|***),
       (***|****),(*|****),(**|***),(*|***),(*|**) :: (Read a, Eq a, LayoutClass l1 a, LayoutClass l2 a)
          => l1 a -> l2 a -> CombineTwo (Tall ()) l1 l2 a
(*|*)      = combineTwo (Tall 1 0.1 (1/2))
(**|*)     = combineTwo (Tall 1 0.1 (2/3))
(***|*)    = combineTwo (Tall 1 0.1 (3/4))
(****|*)   = combineTwo (Tall 1 0.1 (4/5))
(***|**)   = combineTwo (Tall 1 0.1 (3/5))
(****|***) = combineTwo (Tall 1 0.1 (4/7))
(***|****) = combineTwo (Tall 1 0.1 (3/7))
(*|****)   = combineTwo (Tall 1 0.1 (1/5))
(**|***)   = combineTwo (Tall 1 0.1 (2/5))
(*|***)    = combineTwo (Tall 1 0.1 (1/4))
(*|**)     = combineTwo (Tall 1 0.1 (1/3))


-- $mth
-- These combinators combine two layouts horizontally using @Mirror
-- Tall@.

(*/*),(**/*),(***/*),(****/*), (***/**),(****/***),
       (***/****),(*/****),(**/***),(*/***),(*/**) :: (Read a, Eq a, LayoutClass l1 a, LayoutClass l2 a)
          => l1 a -> l2 a -> CombineTwo (Mirror Tall ()) l1 l2 a
(*/*)      = combineTwo (Mirror $ Tall 1 0.1 (1/2))
(**/*)     = combineTwo (Mirror $ Tall 1 0.1 (2/3))
(***/*)    = combineTwo (Mirror $ Tall 1 0.1 (3/4))
(****/*)   = combineTwo (Mirror $ Tall 1 0.1 (4/5))
(***/**)   = combineTwo (Mirror $ Tall 1 0.1 (3/5))
(****/***) = combineTwo (Mirror $ Tall 1 0.1 (4/7))
(***/****) = combineTwo (Mirror $ Tall 1 0.1 (3/7))
(*/****)   = combineTwo (Mirror $ Tall 1 0.1 (1/5))
(**/***)   = combineTwo (Mirror $ Tall 1 0.1 (2/5))
(*/***)    = combineTwo (Mirror $ Tall 1 0.1 (1/4))
(*/**)     = combineTwo (Mirror $ Tall 1 0.1 (1/3))

type NewSelect = Choose
{-# DEPRECATED NewSelect "Use 'Choose' instead." #-}
