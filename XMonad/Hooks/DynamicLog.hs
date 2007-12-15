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
-- DynamicLog
--
-- By default, log events in:
--
-- >     1 2 [3] 4 8
--
-- format, although the format is highly customizable.
-- Suitable to pipe into dzen or xmobar.
--
-----------------------------------------------------------------------------

module XMonad.Hooks.DynamicLog (
    -- * Usage
    -- $usage
    dynamicLog,
    dynamicLogDzen,
    dynamicLogXmobar,
    dynamicLogWithPP,
    dynamicLogXinerama,
    dzen,

    pprWindowSet,
    pprWindowSetXinerama,

    PP(..), defaultPP, dzenPP, sjanssenPP,
    wrap, pad, shorten,
    xmobarColor, dzenColor, dzenEscape,
    makeSimpleDzenConfig
  ) where

--
-- Useful imports
--
import XMonad
import Data.Maybe ( isJust )
import Data.List
import Data.Ord ( comparing )
import qualified XMonad.StackSet as S
import Data.Monoid
import System.IO
import XMonad.Util.NamedWindows
import XMonad.Util.Run
import XMonad.Hooks.UrgencyHook

-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- >    import XMonad
-- >    import XMonad.Hooks.DynamicLog
-- >    main = xmonad defaultConfig { logHook = dynamicLog }

-- | An example xmonad config that spawns a new dzen toolbar and uses the default
-- dynamic log output
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
-- A log function that uses the 'PP' hooks to customize output.
dynamicLogWithPP :: PP -> X ()
dynamicLogWithPP pp = do
    winset <- gets windowset
    urgents <- readUrgents
    spaces <- asks (workspaces . config)
    -- layout description
    let ld = description . S.layout . S.workspace . S.current $ winset
    -- workspace list
    let ws = pprWindowSet spaces urgents pp winset
    -- window title
    wt <- maybe (return "") (fmap show . getName) . S.peek $ winset

    io . ppOutput pp . sepBy (ppSep pp) . ppOrder pp $
                        [ ws
                        , ppLayout pp ld
                        , ppTitle  pp wt
                        ]

-- | An example log hook that emulates dwm's status bar, using colour codes printed to dzen
-- Requires dzen. Workspaces, xinerama, layouts and the window title are handled.
--
dynamicLogDzen :: X ()
dynamicLogDzen = dynamicLogWithPP dzenPP

pprWindowSet :: [String] -> [Window] -> PP -> WindowSet -> String
pprWindowSet spaces urgents pp s =  sepBy (ppWsSep pp) $ map fmt $ sortBy cmp
            (map S.workspace (S.current s : S.visible s) ++ S.hidden s)
   where f Nothing Nothing   = EQ
         f (Just _) Nothing  = LT
         f Nothing (Just _)  = GT
         f (Just x) (Just y) = compare x y

         wsIndex = flip elemIndex spaces . S.tag

         cmp a b = f (wsIndex a) (wsIndex b) `mappend` compare (S.tag a) (S.tag b)

         this     = S.tag (S.workspace (S.current s))
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
-- and 2 and 7 are non-visible, non-empty workspaces
--
dynamicLogXinerama :: X ()
dynamicLogXinerama = withWindowSet $ io . putStrLn . pprWindowSetXinerama

pprWindowSetXinerama :: WindowSet -> String
pprWindowSetXinerama ws = "[" ++ unwords onscreen ++ "] " ++ unwords offscreen
  where onscreen  = map (S.tag . S.workspace)
                        . sortBy (comparing S.screen) $ S.current ws : S.visible ws
        offscreen = map S.tag . filter (isJust . S.stack)
                        . sortBy (comparing S.tag) $ S.hidden ws

wrap :: String -> String -> String -> String
wrap _ _ "" = ""
wrap l r m  = l ++ m ++ r

pad :: String -> String
pad = wrap " " " "

shorten :: Int -> String -> String
shorten n xs | length xs < n = xs
             | otherwise     = (take (n - length end) xs) ++ end
 where
    end = "..."

sepBy :: String -> [String] -> String
sepBy sep = concat . intersperse sep . filter (not . null)

dzenColor :: String -> String -> String -> String
dzenColor fg bg = wrap (fg1++bg1) (fg2++bg2)
 where (fg1,fg2) | null fg = ("","")
                 | otherwise = ("^fg(" ++ fg ++ ")","^fg()")
       (bg1,bg2) | null bg = ("","")
                 | otherwise = ("^bg(" ++ bg ++ ")","^bg()")

-- | Escape any dzen metacharacters.
dzenEscape :: String -> String
dzenEscape = concatMap (\x -> if x == '^' then "^^" else [x])

xmobarColor :: String -> String -> String -> String
xmobarColor fg bg = wrap t "</fc>"
 where t = concat ["<fc=", fg, if null bg then "" else "," ++ bg, ">"]

-- | The 'PP' type allows the user to customize various behaviors of
-- dynamicLogPP
data PP = PP { ppCurrent, ppVisible
             , ppHidden, ppHiddenNoWindows
             , ppUrgent :: WorkspaceId -> String
             , ppSep, ppWsSep :: String
             , ppTitle :: String -> String
             , ppLayout :: String -> String
             , ppOrder :: [String] -> [String]
             , ppOutput :: String -> IO ()
             }

-- | The default pretty printing options, as seen in dynamicLog
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
               }

-- | Settings to emulate dwm's statusbar, dzen only
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
-- 'xmobarColor' and the record update on defaultPP
sjanssenPP :: PP
sjanssenPP = defaultPP { ppCurrent = xmobarColor "white" "#ff000000"
                       , ppTitle = xmobarColor "#00ee00" "" . shorten 80
                       }

-- | These are good defaults to be used with the xmobar status bar
dynamicLogXmobar :: X ()
dynamicLogXmobar =
    dynamicLogWithPP defaultPP { ppCurrent = xmobarColor "yellow" "" . wrap "[" "]"
                               , ppTitle   = xmobarColor "green"  "" . shorten 40
                               , ppVisible = wrap "(" ")"
                               }
