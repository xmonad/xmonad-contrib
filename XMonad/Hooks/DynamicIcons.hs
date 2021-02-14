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
-- a set of utilities that allows easy manipulation of Dynamic Log to allow
-- for dynamic icons
--
-----------------------------------------------------------------------------

module XMonad.Hooks.DynamicIcons (
    -- * Usage
    -- $usage
    
    -- * Creating Dynamic Icons
    dynamicLogIconsWithPP, dynamicLogIconsConvert, 

    -- * Data Types
    appIcon, IconSet,
    IconConfig(..), Icon(..),

    ) where
import XMonad

import qualified XMonad.StackSet as S
import qualified Data.Map as M

import XMonad.Hooks.DynamicLog
import Data.Maybe (catMaybes)

-- $usage
--  Dynamically changes 'WorkspaceId' based on the 'Window's inside the Workspace.
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


-- | Custom datatype for custom icons based on the state of the 'Workspace'
-- For example,
--
-- > Icon "<bold>discord</bold>" "<bold>discord</bold>" "discord" ""
--
-- Then you can add it to your IconSet.
--
-- > icons :: IconSet
-- > icons = mconcat
-- >    [ className =? "discord" --> pure [Icon "<bold>discord</bold>" "<bold>discord</bold>" "discord" ""]
-- >    ]
data Icon = Icon
    { iconCurrent         :: !String -- ^ If the 'Workspace' is the current workspace
    , iconVisible         :: !String -- ^ If the 'Workspace' is visible (Xinerama only)
    , iconHidden          :: !String -- ^ If the 'Workspace' isnt visible but still has windows
    , iconHiddenNoWindows :: !String -- ^ If the 'Workspace' isnt visible and has no windows
    }

-- | The set of Icons to use
type IconSet = Query [Icon]

baseIconSet :: String -> Icon
baseIconSet x =
    Icon { iconCurrent = x
         , iconVisible = x
         , iconHidden = x
         , iconHiddenNoWindows = x
         }

-- | Create an 'IconSet' from a 'String'
appIcon :: String -> IconSet
appIcon x = pure [baseIconSet x]

-- | Adjusts the 'PP' and then runs 'dynamicLogWithPP'
dynamicLogIconsWithPP :: IconSet -- ^ The 'IconSet' to use
                      -> PP -- ^ The 'PP' to alter
                      -> X () -- ^ The resulting 'X' action
dynamicLogIconsWithPP iconset pp = dynamicLogWithPP =<< dynamicLogIconsConvert (def{ iconConfigIcons = iconset, iconConfigPP = pp })

-- | Datatype for expanded 'Icon' configurations
data IconConfig = IconConfig
    { iconConfigIcons :: IconSet -- ^ The 'IconSet' to use
    , iconConfigStack :: [String] -> String -- ^ The function to manage stacking of 'Icon's
    , iconConfigPP    :: PP -- ^ The 'PP' to alter
    }

instance Default IconConfig where
    def = IconConfig
        { iconConfigIcons = mempty
        , iconConfigStack = wrap "[" "]" . unwords
        , iconConfigPP = def
        }

-- | This is the same as 'dynamicLogIconsWithPP' but it takes a 'IconConfig'.
-- This allows you to manually customise the 'Icon's the stacking function and also your `PP`
dynamicLogIconsConvert :: IconConfig -> X PP
dynamicLogIconsConvert iconConfig = do
    ws <- gets (S.workspaces . windowset)
    icons <- M.fromList . catMaybes <$> mapM (getIcons (iconConfigIcons iconConfig)) ws
    pure $ (iconConfigPP iconConfig)
        { ppCurrent = ppSection ppCurrent iconCurrent icons
        , ppVisible = ppSection ppVisible iconVisible icons
        , ppHidden = ppSection ppHidden iconHidden icons
        , ppHiddenNoWindows =  ppSection ppHiddenNoWindows iconHiddenNoWindows icons
        }
  where
    ppSection pF f icons = pF (iconConfigPP iconConfig) . concatIcons f . iconLookup icons
    iconLookup icons x = M.findWithDefault [baseIconSet x] x icons
    concatIcons f y
        | length y > 1 = iconConfigStack iconConfig $ map f y
        | otherwise = concatMap f y


getIcons :: IconSet -> WindowSpace -> X (Maybe (WorkspaceId, [Icon]))
getIcons is w = do
    validIcons <- sequence $ foldMap (runQuery is) . S.integrate <$> S.stack w
    pure $ (S.tag w,) <$> (validIcons >>= \x -> if null x then Nothing else Just x)
