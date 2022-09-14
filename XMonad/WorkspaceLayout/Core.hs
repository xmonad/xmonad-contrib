{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedLabels           #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS_GHC -Wall -Werror #-}

module XMonad.WorkspaceLayout.Core where

import           Prelude                      hiding (span)

import           Control.Category             ((>>>))
import           Data.Function                (on)
import           Data.List                    (elemIndex)
import           GHC.Generics                 (Generic)
import           XMonad                       hiding (config, modify, state,
                                               trace, workspaces)
import           XMonad.Hooks.StatusBar.PP    (PP (..))
import           XMonad.StackSet              (tag)
import           XMonad.Util.WorkspaceCompare (mkWsSort)


-- |
--
-- Encompasses information needed to render a workspace layout
data WorkspaceLayoutView = WSLView
  { label        :: String
  , neighborhood :: [WorkspaceId]
  , toName       :: WorkspaceId -> String
  } deriving (Generic)


render :: WorkspaceLayoutView -> PP
render (WSLView { neighborhood, toName, label }) =

  withLabel . withNameTransform . withNeighborhood $ def

  where

  withNameTransform pp = pp
    { ppCurrent = toName
    , ppHidden = toName
    , ppHiddenNoWindows = toName
    }

  withNeighborhood pp = pp
    { ppSort = do
        sort <- (mkWsSort . pure) (compare `on` flip elemIndex neighborhood)
        pure $ filter (tag >>> (`elem` neighborhood)) >>> sort
    }

  withLabel pp = pp
    { ppOrder = \(workspaces : rest) -> (label <> workspaces) : rest
    }

