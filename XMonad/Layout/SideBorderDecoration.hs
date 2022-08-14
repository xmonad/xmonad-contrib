{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
--------------------------------------------------------------------
-- |
-- Module      : XMonad.Layout.SideBorderDecoration
-- Description : Configure the border position around windows.
-- Copyright   : (c) 2018  L. S. Leary
--                   2022  Tony Zorman
-- License     : BSD3
-- Maintainer  : Tony Zorman <soliditsallgood@mailbox.org>
--
-- This module allows for having a configurable border position around
-- windows; i.e., it can move the border to any cardinal direction.
--
--------------------------------------------------------------------
module XMonad.Layout.SideBorderDecoration (
  -- * Usage
  -- $usage
  sideBorder,

  -- * Border configuration
  SideBorderConfig (..),
  def,

  -- * Re-exports
  Direction2D (..),

  -- * Lower-level hooks
  sideBorderLayout,
) where

import qualified XMonad.StackSet as W

import XMonad
import XMonad.Layout.Decoration
import XMonad.StackSet (Stack)
import XMonad.Util.Types

{- $usage

To use this module, first import it into your configuration file:

> import XMonad.Layout.SideBorderDecoration

You can now add the 'sideBorder' combinator to your configuration:

> main :: IO ()
> main = xmonad
>      $ …
>      $ sideBorder mySideBorderConfig
>      $ def { … }
>  where
>   mySideBorderConfig :: SideBorderConfig
>   mySideBorderConfig = def
>     { sbSide          = D
>     , sbActiveColor   = "#ff0000"
>     , sbInactiveColor = "#ffaaaa"
>     , sbSize          = 5
>     }

or, alternatively,

> main :: IO ()
> main = xmonad
>      $ …
>      $ sideBorder def{ sbSide = D, sbActiveColor = "#ff000", … }
>      $ def { … }

See 'SideBorderConfig' for the different size and colour options.

The following is a fully-functional, minimal configuration example:

> import XMonad
> import XMonad.Layout.SideBorderDecoration
>
> main :: IO ()
> main = xmonad $ sideBorder def $ def

This would result in the following border being displayed:

<<https://user-images.githubusercontent.com/50166980/184537672-136f85a3-dfe7-42e2-b4c8-356d934d1bff.png>>

-}

-----------------------------------------------------------------------
-- Configuration

-- | Configuring how the border looks like.
data SideBorderConfig = SideBorderConfig
  { sbSide          :: !Direction2D  -- ^ Which side to have the border on.
  , sbActiveColor   :: !String       -- ^ Active border colour.
  , sbInactiveColor :: !String       -- ^ Inactive border colour.
  , sbSize          :: !Dimension
    -- ^ Size of the border.  This will be the height if 'sbSide' is 'U'
    --   or 'D' and the width if it is 'L' or 'R'.
  }

instance Default SideBorderConfig where
  def :: SideBorderConfig
  def = SideBorderConfig
    { sbSide          = D
    , sbActiveColor   = "#ff0000"
    , sbInactiveColor = "#ffaaaa"
    , sbSize          = 5
    }

-----------------------------------------------------------------------
-- User-facing

-- | Move the default XMonad border to any of the four cardinal
-- directions.
--
-- Note that this function should only be applied once to your
-- configuration and should /not/ be combined with 'sideBorderLayout'.
sideBorder :: SideBorderConfig -> XConfig l -> XConfig (SideBorder l)
sideBorder sbc cfg =
  cfg{ layoutHook  = sideBorderLayout sbc (layoutHook cfg)
     , borderWidth = 0
     }

-- | Layout hook to only enable the side border for some layouts.  For
-- example:
--
-- > myLayout = Full ||| sideBorderLayout def tall ||| somethingElse
--
-- Note that, unlike 'sideBorder', this does /not/ disable the normal
-- border in XMonad, you will have to do this yourself.  Remove this
-- function from your layout hook and use 'sideBorder' if you want a
-- side border in every layout (do not use the two functions together).
sideBorderLayout :: Eq a => SideBorderConfig -> l a -> SideBorder l a
sideBorderLayout SideBorderConfig{ sbSide, sbActiveColor, sbInactiveColor, sbSize } =
  decoration BorderShrinker theme (SideBorderDecoration sbSide)
 where
  theme :: Theme
  theme = deco
    { activeColor   = sbActiveColor
    , inactiveColor = sbInactiveColor
    }
   where
    deco | sbSide `elem` [U, D] = def{ decoHeight = sbSize }
         | otherwise            = def{ decoWidth  = sbSize }

-----------------------------------------------------------------------
-- Decoration

newtype SideBorderDecoration a = SideBorderDecoration Direction2D
  deriving (Show, Read)

type SideBorder = ModifiedLayout (Decoration SideBorderDecoration BorderShrinker)

instance Eq a => DecorationStyle SideBorderDecoration a where
  shrink :: SideBorderDecoration a -> Rectangle -> Rectangle -> Rectangle
  shrink dec (Rectangle _ _ dw dh) (Rectangle x y w h) = case dec of
    SideBorderDecoration U -> Rectangle x           (y + fi dh) w        (h - dh)
    SideBorderDecoration R -> Rectangle x           y           (w - dw) h
    SideBorderDecoration D -> Rectangle x           y           w        (h - dh)
    SideBorderDecoration L -> Rectangle (x + fi dw) y           (w - dw) h

  pureDecoration
    :: SideBorderDecoration a
    -> Dimension -> Dimension
    -> Rectangle
    -> Stack a
    -> [(a, Rectangle)]
    -> (a, Rectangle)
    -> Maybe Rectangle
  pureDecoration dec dw dh _ st _ (win, Rectangle x y w h)
    | win `elem` W.integrate st && dw < w && dh < h = Just $ case dec of
      SideBorderDecoration U -> Rectangle x                 y                 w  dh
      SideBorderDecoration R -> Rectangle (x + fi (w - dw)) y                 dw h
      SideBorderDecoration D -> Rectangle x                 (y + fi (h - dh)) w  dh
      SideBorderDecoration L -> Rectangle x                 y                 dw h
    | otherwise = Nothing

-----------------------------------------------------------------------
-- Shrinker

-- | Kill all text.
data BorderShrinker = BorderShrinker

instance Show BorderShrinker where
  show :: BorderShrinker -> String
  show _ = ""

instance Read BorderShrinker where
  readsPrec :: Int -> ReadS BorderShrinker
  readsPrec _ s = [(BorderShrinker, s)]

instance Shrinker BorderShrinker where
  shrinkIt :: BorderShrinker -> String -> [String]
  shrinkIt _ _ = [""]
