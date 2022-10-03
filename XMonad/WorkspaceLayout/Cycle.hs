{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# OPTIONS_GHC -Wall -Werror #-}

{- |

Cyclic one-dimensional workspace layouts for XMonad

This module is intended mostly to serve as another example workspace
layout other than 'XMonad.WorkspaceLayout.Grid'.

However, a cyclic layout is not particularly useful, and so this
module isn't /really/ intended to be used. Feel free to if you want,
though. It should still work!

-}

module XMonad.WorkspaceLayout.Cycle
  ( Coord (..)
  , Config (..)
  , BoundsMode (..)
  , move
  , swap
  , hook
  , getView
  ) where

import           Prelude

import           Control.Monad.State         (execState)
import           GHC.Generics                (Generic)
import qualified XMonad
import           XMonad                      hiding (config, state, trace,
                                              workspaces)
import           XMonad.StackSet             (greedyView, shift)

import qualified XMonad.Util.OneState        as St
import           XMonad.Util.OneState        (OneState (..))
import           XMonad.WorkspaceLayout.Core (WorkspaceLayoutView (..))
import           XMonad.WorkspaceLayout.Util (affineMod, (!%))



data Coord = Coord
  { offset   :: Int
  , position :: Int
  }
  deriving (Show, Eq, Ord, Generic)

data Config = Config
  { width      :: Int
  , workspaces :: [WorkspaceId]
  }
  deriving (Show, Generic)

data State = State
  { coord  :: Coord
  , config :: Config
  }
  deriving (Show, Generic)

instance OneState State where
  type Mod State = State -> State
  merge ma s = pure (ma s)
  defaultState = State
    { coord = Coord 0 0
    , config = Config 5 (single <$> ['a' .. 'j'])
    }
    where single = (:[])


data BoundsMode = Clamp | Wrap

move :: BoundsMode -> (Coord -> Coord) -> X ()
move mode f = do
  (coord', wid') <- calc mode f
  St.modify $ \st -> st { coord = coord' }
  windows (greedyView wid')

swap :: BoundsMode -> (Coord -> Coord) -> X ()
swap mode f = do
  (_, wid') <- calc mode f
  windows (shift wid')

calc :: BoundsMode -> (Coord -> Coord) -> X (Coord, WorkspaceId)
calc mode f = do
  State coord (Config { width, workspaces }) <- St.get
  let coord' = flip execState coord $ do
        modify f
        offset' <- offset <$> get
        modify $
          let updatePosition =
                (let lo = offset' - width `div` 2
                     hi = offset' + width `div` 2
                in case mode of
                  Clamp -> max lo . min hi
                  Wrap  -> affineMod (lo, hi))
          in \st -> st { position = updatePosition (position st) }
  let wid = workspaces !% (position coord')
  pure (coord', wid)


hook :: Config -> XConfig l -> XConfig l
hook config = St.once @State
  (\xc -> xc { XMonad.workspaces = workspaces config })
  (\state -> state { config = config })

getView :: X WorkspaceLayoutView
getView = do
  State (Coord { offset }) (Config { width, workspaces }) <- St.get
  pure $ WSLView
    { toName = id
    , label = ""
    , neighborhood =
            (do pos <- [offset - width `div` 2 .. offset + width `div` 2]
                pure $ workspaces !% (pos `mod` length workspaces)
            )
    }

