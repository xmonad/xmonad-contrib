-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Actions.KeyChord
-- Copyright   :  (c) Pedro Rodriguez Tavarez <pedro@pjrt.co>
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  Pedro Rodriguez Tavarez <pedro@pjrt.co>
-- Stability   :  unstable
-- Portability :  unportable
--
-- A module that allows the user to create keychords similar to vi using
-- 'submap' from "XMonad.Actions.Submap".
--
-- What 'keychords' does can be manually (and painfully) replicated using plain
-- 'submap'. This module aims to make the process simpler.
--
-------------------------------------------------------------------------------
module XMonad.Actions.KeyChord
( -- Usage
  -- $usage
  keychords
) where

import Data.PathTree (fromPaths, LCRSTree(Empty, Leaf, Node))
import XMonad
import XMonad.Actions.Submap (submap)
import Data.Map (fromList)

-------------------------------------------------------------------------------

{- $usage
  In your `~/.xmonad/xmonad.hs`, import this module

  @import XMonad.Actions.KeyChord@

  This allows you to do the following:

  @
  , ((modm, xK_semicolon), keychords
      [ ([(0, xK_i), (0, xK_k)], action1)
      , ([(0, xK_i), (0, xK_w)], action2)
      , ([(0, xK_j), (0, xK_j)], action3)
      , ([(0, xK_s), (0, xK_a), (shiftMask, xK_t)], action4)
      ])
  @

  Now, to run @action1@, press /modm-; i k/.

  Restrictions:

  * If two chords conflict (ie: are the same), the last one will be the active
    one
  * If a chord is a subset of another (eg: k -> t -> y and k -> t), the shorter
    one will be picked

-}

-- | Given a list of tuples of key strokes and an action, collect all the
-- keystrokes and map them to the action using 'submap' from
-- "XMonad.Actions.Submap".
keychords :: [([(KeyMask, KeySym)], X ())] -> X ()
keychords kCombos =
  let trees = fromPaths $ map tupLast kCombos
  in sm . buildChord $ fmap toEither trees
  where
    tupLast :: ([n], a) -> [NEither n a]
    tupLast (ps, a) =
      map (NEither . Left) (init ps) ++ [NEither $ Right (last ps, a)]

    buildChord Empty = []
    buildChord (Leaf (Right (n, a)) s) = (n, a) : buildChord s
    buildChord (Node (Left n) c s) = (n, sm $ buildChord c) : buildChord s
    buildChord Leaf {} = error "buildChord: incorrectly built tree. A leaf with a Left"
    buildChord Node {} = error "buildChord: incorrectly built tree. A node with a Right"

    sm = submap . fromList

-- | A version of Either where only 'n' has to match in order to count as
-- "equal". Used in 'keychords' to produce the chord paths using the @LCRSTree@.
newtype NEither n a = NEither { toEither :: Either n (n, a) }

instance Eq n => Eq (NEither n a) where
  (NEither (Left n1)) == (NEither (Left n2)) = n1 == n2
  (NEither (Right (n1, _))) == (NEither (Right (n2, _))) = n1 == n2
  _ == _ = False
