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
  def
    -- display the workspace names
    { ppCurrent = toName
    , ppHidden = toName
    , ppHiddenNoWindows = toName

    -- display only a subset of workspaces (the "neighborhood") of the current workspace
    , ppSort = do
        sort <- (mkWsSort . pure) (compare `on` flip elemIndex neighborhood)
        pure $ filter (tag >>> (`elem` neighborhood)) >>> sort

    -- display the label to the left
    , ppOrder = \(workspaces : rest) -> (label <> workspaces) : rest
    }

