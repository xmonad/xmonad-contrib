{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Hooks.DynamicIcons
-- Copyright   :  (c) Will Pierlot <willp@outlook.com.au>
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  Will Pierlot <willp@outlook.com.au>
-- Stability   :  unstable
-- Portability :  unportable
--
-- Dynamically change workspace text based on the contents of the workspace
-----------------------------------------------------------------------------

module XMonad.Hooks.DynamicIcons (
    -- * Usage
    -- $usage

    -- * Creating Dynamic Icons
    dynamicLogIconsWithPP, appIcon,

    -- * Customization
    dynamicIconsPP, getWorkspaceIcons,
    IconConfig(..),
    iconsFmtAppend, iconsFmtReplace, wrapUnwords,

    ) where
import XMonad

import qualified XMonad.StackSet as S
import qualified Data.Map as M

import XMonad.Hooks.DynamicLog
import Data.Functor ((<&>))
import Data.Traversable (for)
import Control.Monad ((<=<), (>=>))

-- $usage
--  Dynamically changes a 'Workspace's 'WorkspaceId' based on the 'Window's inside the Workspace.
-- 'IconSet's describe which icons are shown depending on which windows fit a 'Query'.
--
-- To create an 'IconSet' make a 'Query' that returns a ['Icon'].
--
-- 'appIcon' can be used to simplify this process
-- For example,
--
-- > icons :: IconSet
-- > icons = composeAll
-- >           [ className =? "discord" --> appIcon "\xfb6e"
-- >           , className =? "Discord" --> appIcon "\xf268"
-- >           , className =? "Firefox" --> appIcon "\63288"
-- >           , className =? "Spotify" <||> className =? "spotify" --> appIcon "阮"
-- >           ]
--
-- then you can add the hook to your config
--
-- > xmonad $ def
-- >      { logHook = dynamicLogIconsWithPP icons xmobarPP <> myManageHook
-- >      }
--
--  Here is an example of this
--
--  <<https://imgur.com/download/eauPNPz/Dynamic%20Icons%20in%20XMonad>>
--  
--  NOTE: You can use any string you want here. The example shown here, uses NerdFont Icons to represent open applications


-- | Shortcut for configuring single icons.
appIcon :: String -> Query [String]
appIcon = pure . pure

-- | Adjusts the 'PP' and then runs 'dynamicLogWithPP'
dynamicLogIconsWithPP :: Query [String] -- ^ The 'IconSet' to use
                      -> PP -- ^ The 'PP' to alter
                      -> X () -- ^ The resulting 'X' action
dynamicLogIconsWithPP q = dynamicLogWithPP <=< dynamicIconsPP def{ iconConfigIcons = q }

-- | Datatype for expanded 'Icon' configurations
data IconConfig = IconConfig
    { iconConfigIcons :: Query [String]
      -- ^ What icons to use for each window.
    , iconConfigFmt :: WorkspaceId -> [String] -> String
      -- ^ How to format the result, see 'iconsFmtReplace', 'iconsFmtAppend'.
    }

instance Default IconConfig where
    def = IconConfig
        { iconConfigIcons = mempty
        , iconConfigFmt = iconsFmtReplace (wrapUnwords "{" "}")
        }

-- | 'iconConfigFmt' that replaces the workspace name with icons, if any.
--
-- First parameter specifies how to concatenate multiple icons. Useful values
-- include: 'concat', 'unwords', 'wrapUnwords'.
--
-- >>> iconsFmtReplace concat "1" []
-- "1"
--
-- >>> iconsFmtReplace concat "1" ["A", "B"]
-- "AB"
--
-- >>> iconsFmtReplace (wrapUnwords "{" "}") "1" ["A", "B"]
-- "{A B}"
iconsFmtReplace :: ([String] -> String) -> WorkspaceId -> [String] -> String
iconsFmtReplace cat ws is | null is   = ws
                          | otherwise = cat is

-- | 'iconConfigFmt' that appends icons to the workspace name.
--
-- First parameter specifies how to concatenate multiple icons. Useful values
-- include: 'concat', 'unwords', 'wrapUnwords'.
--
-- >>> iconsFmtAppend concat "1" []
-- "1"
--
-- >>> iconsFmtAppend concat "1" ["A", "B"]
-- "1:AB"
iconsFmtAppend :: ([String] -> String) -> WorkspaceId -> [String] -> String
iconsFmtAppend cat ws is | null is   = ws
                         | otherwise = ws ++ ':' : cat is

-- | Join words with spaces, and wrap the result in delimiters unless there
-- was exactly one element.
--
-- >>> wrapUnwords "{" "}" ["A", "B"]
-- "{A B}"
--
-- >>> wrapUnwords "{" "}" ["A"]
-- "A"
--
-- >>> wrapUnwords "{" "}" []
-- ""
wrapUnwords :: String -> String -> [String] -> String
wrapUnwords _ _ [x] = x
wrapUnwords l r xs  = wrap l r (unwords xs)

-- | Modify "XMonad.Hooks.DynamicLog"\'s pretty-printing format to augment
-- workspace names with icons based on the contents (windows) of the workspace.
dynamicIconsPP :: IconConfig -> PP -> X PP
dynamicIconsPP ic pp = getWorkspaceIcons ic <&> \ren -> pp{ ppRename = ppRename pp >=> ren }

-- | Returns a function for 'ppRename' that augments workspaces with icons
-- according to the provided 'IconConfig'.
getWorkspaceIcons :: IconConfig -> X (String -> WindowSpace -> String)
getWorkspaceIcons IconConfig{..} = fmt <$> getWorkspaceIcons' iconConfigIcons
  where
    fmt icons s w = iconConfigFmt s (M.findWithDefault [] (S.tag w) icons)

getWorkspaceIcons' :: Query [String] -> X (M.Map WorkspaceId [String])
getWorkspaceIcons' q = do
    ws <- gets (S.workspaces . windowset)
    is <- for ws $ foldMap (runQuery q) . S.integrate' . S.stack
    pure $ M.fromList (zip (map S.tag ws) is)
