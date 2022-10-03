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
-- If you're just getting up-and-running, prefer 'render'' for now.
--
-- The result @PP@ will not add any styling to differentiate focused/hidden/etc
-- windows. You will have to add additional modifications on top (or just
-- use 'render''). When doing so, take care not to overwrite
-- what's already been set. For instance, instead of doing:
--
-- > (render view) { ppHidden = myHidden }
--
-- prefer
--
-- > let rendered = render view
-- > in rendered { ppHidden = myHidden . rendered }
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

-- | Like 'render' but with some defaults for how to display focused/hidden/etc windows
render' :: WorkspaceLayoutView -> PP
render' wsl =
  let pp = render wsl in pp
    { ppCurrent = ppCurrent def . ppCurrent pp
    , ppHidden = ppHidden def . ppHidden pp
    , ppHiddenNoWindows = (map (const '.')) . ppHiddenNoWindows pp
    }
