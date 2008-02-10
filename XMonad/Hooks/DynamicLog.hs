-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Hooks.DynamicLog
-- Copyright   :  (c) Don Stewart <dons@cse.unsw.edu.au>
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  Don Stewart <dons@cse.unsw.edu.au>
-- Stability   :  unstable
-- Portability :  unportable
--
-- xmonad calls the logHook with every internal state update, which is
-- useful for (among other things) outputting status information to an
-- external status bar program such as xmobar or dzen.  DynamicLog
-- provides several drop-in logHooks for this purpose, as well as
-- flexible tools for specifying your own formatting.
--
-----------------------------------------------------------------------------

module XMonad.Hooks.DynamicLog (
    -- * Usage
    -- $usage

    -- * Drop-in loggers
    makeSimpleDzenConfig,
    dzen,
    dynamicLog,
    dynamicLogString,
    dynamicLogDzen,
    dynamicLogXmobar,
    dynamicLogWithPP,
    dynamicLogXinerama,

    -- * Build your own formatter
    PP(..), defaultPP, dzenPP, sjanssenPP, byorgeyPP,

    -- * Formatting utilities
    wrap, pad, shorten,
    xmobarColor, dzenColor, dzenEscape,

    -- * Internal formatting functions
    pprWindowSet,
    pprWindowSetXinerama

  ) where

--
-- Useful imports
--
import XMonad
import Data.Maybe ( isJust )
import Data.List
import Data.Ord ( comparing )
import qualified XMonad.StackSet as S
import System.IO
import XMonad.Util.WorkspaceCompare
import XMonad.Util.NamedWindows
import XMonad.Util.Run
import XMonad.Hooks.UrgencyHook

-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- >    import XMonad
-- >    import XMonad.Hooks.DynamicLog
--
-- Then set your logHook to an appropriate function, for example
--
-- >    logHook = dynamicLog
--
-- or, for more flexibility, something like
--
-- >    logHook = dynamicLogWithPP myDynamicLogPP
-- > ...
-- > myDynamicLogPP = defaultPP { ... -- override pretty-printer with specific settings
--
-- If you don't use statusbar, you can use dynamicLogString to show on-screen
-- notifications in response to some events. E.g. to show current layout when
-- it's changed create apropriate PP and add to keybindings:
--
-- >    , ((mod1Mask, xK_a     ), sendMessage NextLayout >> (dynamicLogString myPP >>= \d->spawn $"xmessage "++d))

-- | An example xmonad config that spawns a new dzen toolbar and uses
--   the default dynamic log output.
makeSimpleDzenConfig :: IO (XConfig (Choose Tall (Choose (Mirror Tall) Full)))
makeSimpleDzenConfig = do
  h <- spawnPipe "dzen2"
  return defaultConfig
           { defaultGaps = [(18,0,0,0)]
           , logHook = dynamicLogWithPP dzenPP
                                          { ppOutput = hPutStrLn h } }

-- |
--
-- Run xmonad with a dzen status bar set to some nice defaults. Output
-- is taken from the dynamicLogWithPP hook.
--
-- > main = dzen xmonad
--
-- The intent is that the above config file should provide a nice status
-- bar with minimal effort.
--
dzen :: (XConfig (Choose Tall (Choose (Mirror Tall) Full)) -> IO ()) -> IO ()
dzen f = do
  h <- spawnPipe ("dzen2" ++ " " ++ flags)
  f $ defaultConfig
           { defaultGaps = [(15,0,0,0)] -- for fixed
           , logHook = dynamicLogWithPP dzenPP
                          { ppOutput = hPutStrLn h } }
 where
    fg      = "'#a8a3f7'" -- n.b quoting
    bg      = "'#3f3c6d'"
    flags   = "-e '' -w 400 -ta l -fg " ++ fg ++ " -bg " ++ bg

-- |
-- An example log hook, print a status bar output to stdout, in the form:
--
-- > 1 2 [3] 4 7 : full : title
--
-- That is, the currently populated workspaces, the current
-- workspace layout, and the title of the focused window.
--
dynamicLog :: X ()
dynamicLog = dynamicLogWithPP defaultPP

-- |
-- Returns formatted log message.
dynamicLogString :: PP -> X String
dynamicLogString pp = do
    winset <- gets windowset
    urgents <- readUrgents
    sort' <- ppSort pp
    -- layout description
    let ld = description . S.layout . S.workspace . S.current $ winset
    -- workspace list
    let ws = pprWindowSet sort' urgents pp winset
    -- window title
    wt <- maybe (return "") (fmap show . getName) . S.peek $ winset

    return $ sepBy (ppSep pp) . ppOrder pp $
                        [ ws
                        , ppLayout pp ld
                        , ppTitle  pp wt
                        ]

-- |
-- A log function that uses the 'PP' hooks to customize output.
dynamicLogWithPP :: PP -> X ()
dynamicLogWithPP pp = dynamicLogString pp >>= io . ppOutput pp 

-- | An example log hook that emulates dwm's status bar, using colour
-- codes printed to dzen.  Requires dzen. Workspaces, xinerama,
-- layouts and the window title are handled.
--
dynamicLogDzen :: X ()
dynamicLogDzen = dynamicLogWithPP dzenPP

-- | Do the actual status formatting, using a pretty-printer.
pprWindowSet :: ([WindowSpace] -> [WindowSpace]) -> [Window] -> PP -> WindowSet -> String
pprWindowSet sort' urgents pp s = sepBy (ppWsSep pp) . map fmt . sort' $
            map S.workspace (S.current s : S.visible s) ++ S.hidden s
   where this     = S.tag (S.workspace (S.current s))
         visibles = map (S.tag . S.workspace) (S.visible s)

         fmt w = printer pp (S.tag w)
          where printer | S.tag w == this                                               = ppCurrent
                        | S.tag w `elem` visibles                                       = ppVisible
                        | any (\x -> maybe False (== S.tag w) (S.findTag x s)) urgents  = \ppC -> ppUrgent ppC . ppHidden ppC
                        | isJust (S.stack w)                                            = ppHidden
                        | otherwise                                                     = ppHiddenNoWindows

-- |
-- Workspace logger with a format designed for Xinerama:
--
-- > [1 9 3] 2 7
--
-- where 1, 9, and 3 are the workspaces on screens 1, 2 and 3, respectively,
-- and 2 and 7 are non-visible, non-empty workspaces.
--
dynamicLogXinerama :: X ()
dynamicLogXinerama = withWindowSet $ io . putStrLn . pprWindowSetXinerama

pprWindowSetXinerama :: WindowSet -> String
pprWindowSetXinerama ws = "[" ++ unwords onscreen ++ "] " ++ unwords offscreen
  where onscreen  = map (S.tag . S.workspace)
                        . sortBy (comparing S.screen) $ S.current ws : S.visible ws
        offscreen = map S.tag . filter (isJust . S.stack)
                        . sortBy (comparing S.tag) $ S.hidden ws

-- | Wrap a string in delimiters, unless it is empty.
wrap :: String  -- ^ left delimiter
     -> String  -- ^ right delimiter
     -> String  -- ^ output string
     -> String
wrap _ _ "" = ""
wrap l r m  = l ++ m ++ r

-- | Pad a string with a leading and trailing space.
pad :: String -> String
pad = wrap " " " "

-- | Limit a string to a certain length, adding "..." if truncated.
shorten :: Int -> String -> String
shorten n xs | length xs < n = xs
             | otherwise     = (take (n - length end) xs) ++ end
 where
    end = "..."

-- | Output a list of strings, ignoring empty ones and separating the
--   rest with the given separator.
sepBy :: String   -- ^ separator
      -> [String] -- ^ fields to output
      -> String
sepBy sep = concat . intersperse sep . filter (not . null)

-- | Use dzen escape codes to output a string with given foreground
--   and background colors.
dzenColor :: String  -- ^ foreground color: a color name, or #rrggbb format
          -> String  -- ^ background color
          -> String  -- ^ output string
          -> String
dzenColor fg bg = wrap (fg1++bg1) (fg2++bg2)
 where (fg1,fg2) | null fg = ("","")
                 | otherwise = ("^fg(" ++ fg ++ ")","^fg()")
       (bg1,bg2) | null bg = ("","")
                 | otherwise = ("^bg(" ++ bg ++ ")","^bg()")

-- | Escape any dzen metacharacters.
dzenEscape :: String -> String
dzenEscape = concatMap (\x -> if x == '^' then "^^" else [x])

-- | Use xmobar escape codes to output a string with given foreground
--   and background colors.
xmobarColor :: String  -- ^ foreground color: a color name, or #rrggbb format
            -> String  -- ^ background color
            -> String  -- ^ output string
            -> String
xmobarColor fg bg = wrap t "</fc>"
 where t = concat ["<fc=", fg, if null bg then "" else "," ++ bg, ">"]

-- | The 'PP' type allows the user to customize various behaviors of
--   dynamicLogPP.
data PP = PP { ppCurrent :: WorkspaceId -> String
               -- ^ how to print the tag of the currently focused workspace
             , ppVisible :: WorkspaceId -> String
               -- ^ how to print tags of visible but not focused workspaces (xinerama only)
             , ppHidden  :: WorkspaceId -> String
               -- ^ how to print tags of hidden workspaces which contain windows
             , ppHiddenNoWindows :: WorkspaceId -> String
               -- ^ how to print tags of empty hidden workspaces
             , ppUrgent :: WorkspaceId -> String
               -- ^ format to be applied to tags of urgent workspaces.
               -- NOTE that 'ppUrgent' is applied /in addition to/ 'ppHidden'!
             , ppSep :: String
               -- ^ separator to use between different log sections (window name, layout, workspaces)
             , ppWsSep :: String
               -- ^ separator to use between workspace tags
             , ppTitle :: String -> String
               -- ^ window title format
             , ppLayout :: String -> String
               -- ^ layout name format
             , ppOrder :: [String] -> [String]
               -- ^ how to order the different log sections
             , ppSort :: X ([WindowSpace] -> [WindowSpace])
               -- ^ how to sort the workspaces.  See "XMonad.Util.WorkspaceCompare" for some useful sorts.
             , ppOutput :: String -> IO ()
               -- ^ formatter that gets applied to the entire log string before it is output.
             }

-- | The default pretty printing options, as seen in 'dynamicLog'.
defaultPP :: PP
defaultPP = PP { ppCurrent         = wrap "[" "]"
               , ppVisible         = wrap "<" ">"
               , ppHidden          = id
               , ppHiddenNoWindows = const ""
               , ppUrgent          = id
               , ppSep             = " : "
               , ppWsSep           = " "
               , ppTitle           = shorten 80
               , ppLayout          = id
               , ppOrder           = id
               , ppOutput          = putStrLn
               , ppSort            = getSortByIndex
               }

-- | Settings to emulate dwm's statusbar, dzen only.
dzenPP :: PP
dzenPP = defaultPP { ppCurrent  = dzenColor "white" "#2b4f98" . pad
                     , ppVisible  = dzenColor "black" "#999999" . pad
                     , ppHidden   = dzenColor "black" "#cccccc" . pad
                     , ppHiddenNoWindows = const ""
                     , ppUrgent   = dzenColor "red" "yellow"
                     , ppWsSep    = ""
                     , ppSep      = ""
                     , ppLayout   = dzenColor "black" "#cccccc" .
                                    (\ x -> case x of
                                              "TilePrime Horizontal" -> " TTT "
                                              "TilePrime Vertical"   -> " []= "
                                              "Hinted Full"          -> " [ ] "
                                              _                      -> pad x
                                    )
                     , ppTitle    = ("^bg(#324c80) " ++) . dzenEscape
                     }

-- | The options that sjanssen likes to use, as an example.  Note the use of
-- 'xmobarColor' and the record update on 'defaultPP'.
sjanssenPP :: PP
sjanssenPP = defaultPP { ppCurrent = xmobarColor "white" "#ff000000"
                       , ppTitle = xmobarColor "#00ee00" "" . shorten 80
                       }

-- | The options that byorgey likes to use with dzen, as another example.
byorgeyPP :: PP
byorgeyPP = defaultPP { ppHiddenNoWindows = showNamedWorkspaces
                      , ppHidden  = dzenColor "black"  "#a8a3f7" . pad
                      , ppCurrent = dzenColor "yellow" "#a8a3f7" . pad
                      , ppUrgent  = dzenColor "red"    "yellow"
                      , ppSep     = " | "
                      , ppWsSep   = ""
                      , ppTitle   = shorten 70
                      , ppOrder   = reverse
                      }
  where showNamedWorkspaces wsId = if any (`elem` wsId) ['a'..'z']
                                       then pad wsId
                                       else ""

-- | These are good defaults to be used with the xmobar status bar.
dynamicLogXmobar :: X ()
dynamicLogXmobar =
    dynamicLogWithPP defaultPP { ppCurrent = xmobarColor "yellow" "" . wrap "[" "]"
                               , ppTitle   = xmobarColor "green"  "" . shorten 40
                               , ppVisible = wrap "(" ")"
                               }
