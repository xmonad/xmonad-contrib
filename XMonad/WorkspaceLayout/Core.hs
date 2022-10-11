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

{- |

Generic operations for workspace layouts

See 'XMonad.WorkspaceLayout.Grid'

-}

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


-- |
--
-- Render a workspace layout to a 'PP'
--
-- When modifying the result 'PP', be careful when overwriting any of
--
-- * 'ppRename'
-- * 'ppSort'
-- * 'ppOrder'
--
-- as the provided implementations are designed to make the workspace
-- layout perform as expected
render :: WorkspaceLayoutView -> PP
render (WSLView { neighborhood, toName, label }) =
  def
    -- display the workspace names
    { ppRename = const . toName

    -- display hidden windows as ellipses because blanking out hidden
    -- windows doesn't make as much sense in the grid paradigm
    , ppHiddenNoWindows = map (const '.')

    -- display only a subset of workspaces (the "neighborhood") of the current workspace
    , ppSort = do
        sort <- (mkWsSort . pure) (compare `on` flip elemIndex neighborhood)
        pure $ filter (tag >>> (`elem` neighborhood)) >>> sort

    -- display the label to the left
    , ppOrder = \(workspaces : rest) -> (label <> workspaces) : rest
    }
