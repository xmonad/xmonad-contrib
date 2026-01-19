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


-- | Render a workspace layout onto an existing 'PP'
modPPWithWorkspaceLayout :: WorkspaceLayoutView -> (PP -> PP)
modPPWithWorkspaceLayout (WSLView { neighborhood, toName, label }) pp =
  pp
    -- display the workspace names
    { ppRename = (fmap . fmap) toName (ppRename pp)

    -- display only a subset of workspaces (the "neighborhood") of the current workspace
    , ppSort = do
        oldSort <- ppSort pp
        newSort <- do
          sortIt <- (mkWsSort . pure) (compare `on` flip elemIndex neighborhood)
          let filterIt = filter (tag >>> (`elem` neighborhood))
          pure $ filterIt >>> sortIt
        pure $ newSort . oldSort

    -- display label to the left
    , ppOrder = ppOrder pp >>> (\(ws : rest) -> (label <> ws) : rest)
    }
