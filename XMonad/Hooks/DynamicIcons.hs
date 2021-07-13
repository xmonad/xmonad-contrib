{-# LANGUAGE RecordWildCards #-}
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
-- Dynamically augment workspace names logged to a status bar via DynamicLog
-- based on the contents (windows) of the workspace.
-----------------------------------------------------------------------------

module XMonad.Hooks.DynamicIcons (
    -- * Usage
    -- $usage

    -- * Creating Dynamic Icons
    dynamicLogIconsWithPP, appIcon,

    -- * Customization
    dynamicIconsPP, getWorkspaceIcons, getWorkspaceIcons', getFocusedIcon,
    IconConfig(..),
    iconsFmtAppend, iconsFmtReplace, wrapUnwords,

    ) where
import           XMonad

import qualified Data.Map                as M
import qualified XMonad.StackSet         as S

import           XMonad.Hooks.DynamicLog
import           XMonad.Prelude          (for, maybeToList, (<&>), (<=<), (>=>))

-- $usage
-- Dynamically augment Workspace's 'WorkspaceId' as shown on a status bar
-- based on the 'Window's inside the Workspace.
--
-- Icons are specified by a @Query [String]@, which is something like a
-- 'ManageHook' (and uses the same syntax) that returns a list of 'String's
-- (icons). This 'Query' is evaluated for each window and the results are
-- joined together. 'appIcon' is a useful shortcut here.
--
-- For example:
--
-- > myIcons :: Query [String]
-- > myIcons = composeAll
-- >   [ className =? "discord" --> appIcon "\xfb6e"
-- >   , className =? "Discord" --> appIcon "\xf268"
-- >   , className =? "Firefox" --> appIcon "\63288"
-- >   , className =? "Spotify" <||> className =? "spotify" --> appIcon "阮"
-- >   ]
--
-- then you can add the hook to your config:
--
-- > main = xmonad $ … $ def
-- >   { logHook = dynamicLogIconsWithPP icons xmobarPP
-- >   , … }
--
-- Here is an example of this
--
-- <<https://user-images.githubusercontent.com/300342/111010930-36a54300-8398-11eb-8aec-b3059b04fa31.png>>
--
-- Note: You can use any string you want here.
-- The example shown here uses NerdFont Icons to represent open applications.
--
-- If you want to customize formatting and/or combine this with other
-- 'PP' extensions like "XMonad.Util.ClickableWorkspaces", here's a more
-- advanced example how to do that:
--
-- > myIconConfig = def{ iconConfigIcons = myIcons, iconConfigFmt = iconsFmtAppend concat }
-- > main = xmonad $ … $ def
-- >   { logHook = xmonadPropLog =<< dynamicLogString =<< clickablePP =<<
-- >               dynamicIconsPP myIconConfig xmobarPP
-- >   , … }


-- | Shortcut for configuring single icons.
appIcon :: String -> Query [String]
appIcon = pure . pure

-- | Adjusts the 'PP' and then runs 'dynamicLogWithPP'
dynamicLogIconsWithPP :: Query [String] -- ^ The 'IconSet' to use
                      -> PP -- ^ The 'PP' to alter
                      -> X () -- ^ The resulting 'X' action
dynamicLogIconsWithPP q = dynamicLogWithPP <=< dynamicIconsPP def{ iconConfigIcons = q }

-- | Modify "XMonad.Hooks.DynamicLog"\'s pretty-printing format to augment
-- workspace names with icons based on the contents (windows) of the workspace.
dynamicIconsPP :: IconConfig -> PP -> X PP
dynamicIconsPP ic pp = getWorkspaceIcons ic <&> \ren -> pp{ ppRename = ppRename pp >=> ren }

-- | Returns a function for 'ppRename' that augments workspaces with icons
-- according to the provided 'IconConfig'.
getWorkspaceIcons :: IconConfig -> X (String -> WindowSpace -> String)
getWorkspaceIcons IconConfig{..} = fmt <$> iconFilterFunction iconConfigIcons
  where
    fmt icons s w = iconConfigFmt s (M.findWithDefault [] (S.tag w) icons)

-- | Only use the focused window for each workspace to find icon
getFocusedIcon :: Query [String] -> X (M.Map WorkspaceId [String])
getFocusedIcon q = do
    ws <- gets (S.workspaces . windowset)
    is <- for ws $  fmap (concat . maybeToList) . traverse (runQuery q . S.focus)  . S.stack
    pure $ M.fromList (zip (map S.tag ws) is)

-- | Use all icons for each workspace
getWorkspaceIcons' :: Query [String] -> X (M.Map WorkspaceId [String])
getWorkspaceIcons' q = do
    ws <- gets (S.workspaces . windowset)
    is <- flip foldMap ws $ traverse (runQuery q .  S.focus) . maybeToList . S.stack
    pure $ M.fromList (zip (map S.tag ws) is)


-- | Datatype for expanded 'Icon' configurations
data IconConfig = IconConfig
    { iconConfigIcons    :: Query [String]
      -- ^ What icons to use for each window.
    , iconConfigFmt      :: WorkspaceId -> [String] -> String
      -- ^ How to format the result, see 'iconsFmtReplace', 'iconsFmtAppend'.
    , iconFilterFunction :: Query [String] -> X (M.Map WorkspaceId [String])
      -- ^ How to find the appropriate icons for each workspace
    }

instance Default IconConfig where
    def = IconConfig
        { iconConfigIcons = mempty
        , iconConfigFmt = iconsFmtReplace (wrapUnwords "{" "}")
        , iconFilterFunction = getWorkspaceIcons'
        }

-- | 'iconConfigFmt' that replaces the workspace name with icons, if any.
--
-- First parameter specifies how to concatenate multiple icons. Useful values
-- include: 'concat', 'unwords', 'wrapUnwords'.
--
-- ==== __Examples__
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
-- ==== __Examples__
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
-- ==== __Examples__
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
 (unwords xs)
