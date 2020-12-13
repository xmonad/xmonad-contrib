{-# LANGUAGE FlexibleContexts, PatternGuards #-}

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
    dzen,
    dzenWithFlags,
    xmobar,
    statusBar,
    statusBar',
    dynamicLog,
    dynamicLogXinerama,

    xmonadPropLog',
    xmonadPropLog,

    -- * Build your own formatter
    dynamicLogWithPP,
    dynamicLogString,
    PP(..), def,

    -- * Example formatters
    dzenPP, xmobarPP, sjanssenPP, byorgeyPP,

    -- * Formatting utilities
    wrap, pad, trim, shorten, shorten', shortenLeft, shortenLeft',
    xmobarColor, xmobarAction, xmobarBorder,
    xmobarRaw, xmobarStrip, xmobarStripTags,
    dzenColor, dzenEscape, dzenStrip, filterOutWsPP,

    -- * Internal formatting functions
    pprWindowSet,
    pprWindowSetXinerama

    -- * To Do
    -- $todo

  ) where

-- Useful imports

import Codec.Binary.UTF8.String (encodeString)
import Control.Applicative (liftA2)
import Control.Monad (msum)
import Data.Char ( isSpace, ord )
import Data.List (intersperse, stripPrefix, isPrefixOf, sortBy)
import Data.Maybe ( isJust, catMaybes, mapMaybe, fromMaybe )
import Data.Ord ( comparing )
import qualified Data.Map as M
import qualified XMonad.StackSet as S

import Foreign.C (CChar)

import XMonad

import XMonad.Util.WorkspaceCompare
import XMonad.Util.NamedWindows
import XMonad.Util.Run

import XMonad.Layout.LayoutModifier
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.ManageDocks

-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- >    import XMonad
-- >    import XMonad.Hooks.DynamicLog
--
-- If you just want a quick-and-dirty status bar with zero effort, try
-- the 'xmobar' or 'dzen' functions:
--
-- > main = xmonad =<< xmobar myConfig
-- >
-- > myConfig = def { ... }
--
-- There is also 'statusBar' if you'd like to use another status bar, or would
-- like to use different formatting options.  The 'xmobar', 'dzen', and
-- 'statusBar' functions are preferred over the other options listed below, as
-- they take care of all the necessary plumbing -- no shell scripting required!
--
-- Alternatively, you can choose among several default status bar formats
-- ('dynamicLog' or 'dynamicLogXinerama') by simply setting your logHook to the
-- appropriate function, for instance:
--
-- > main = xmonad $ def {
-- >    ...
-- >    logHook = dynamicLog
-- >    ...
-- >  }
--
-- For more flexibility, you can also use 'dynamicLogWithPP' and supply
-- your own pretty-printing format (by either defining one from scratch,
-- or customizing one of the provided examples).
-- For example:
--
-- >    -- use sjanssen's pretty-printer format, but with the sections
-- >    -- in reverse
-- >    logHook = dynamicLogWithPP $ sjanssenPP { ppOrder = reverse }
--
-- Note that setting the @logHook@ only sets up xmonad's output; you
-- are responsible for starting your own status bar program (e.g. dzen
-- or xmobar) and making sure xmonad's output is piped into it
-- appropriately, either by putting it in your @.xsession@ or similar
-- file, or by using @spawnPipe@ in your @main@ function, for example:
--
-- > import XMonad.Util.Run   -- for spawnPipe and hPutStrLn
-- >
-- > main = do
-- >     h <- spawnPipe "xmobar -options -foo -bar"
-- >     xmonad $ def {
-- >       ...
-- >       logHook = dynamicLogWithPP $ def { ppOutput = hPutStrLn h }
--
-- If you use @spawnPipe@, be sure to redefine the 'ppOutput' field of
-- your pretty-printer as in the example above; by default the status
-- will be printed to stdout rather than the pipe you create.
--
-- Even if you don't use a statusbar, you can still use
-- 'dynamicLogString' to show on-screen notifications in response to
-- some events. For example, to show the current layout when it
-- changes, you could make a keybinding to cycle the layout and
-- display the current status:
--
-- >    , ((mod1Mask, xK_a     ), sendMessage NextLayout >> (dynamicLogString myPP >>= \d->spawn $"xmessage "++d))
--

-- $todo
--
--   * incorporate dynamicLogXinerama into the PP framework somehow
--
--   * add an xmobarEscape function

------------------------------------------------------------------------

-- | Run xmonad with a dzen status bar with specified dzen
--   command line arguments.
--
-- > main = xmonad =<< dzenWithFlags flags myConfig
-- >
-- > myConfig = def { ... }
-- >
-- > flags = "-e onstart lower -w 800 -h 24 -ta l -fg #a8a3f7 -bg #3f3c6d"
--
-- This function can be used to customize the arguments passed to dzen2.
-- e.g changing the default width and height of dzen2.
--
-- If you wish to customize the status bar format at all, you'll have to
-- use the 'statusBar' function instead.
--
-- The binding uses the XMonad.Hooks.ManageDocks module to automatically
-- handle screen placement for dzen, and enables 'mod-b' for toggling
-- the menu bar.
--
-- You should use this function only when the default 'dzen' function does not
-- serve your purpose.
--
dzenWithFlags :: LayoutClass l Window
    => String -> XConfig l -> IO (XConfig (ModifiedLayout AvoidStruts l))
dzenWithFlags flags conf = statusBar ("dzen2 " ++ flags) dzenPP toggleStrutsKey conf

-- | Run xmonad with a dzen status bar set to some nice defaults.
--
-- > main = xmonad =<< dzen myConfig
-- >
-- > myConfig = def { ... }
--
-- The intent is that the above config file should provide a nice
-- status bar with minimal effort.
--
-- The binding uses the XMonad.Hooks.ManageDocks module to automatically
-- handle screen placement for dzen, and enables 'mod-b' for toggling
-- the menu bar. Please refer to 'dzenWithFlags' function for further
-- documentation.
--
dzen :: LayoutClass l Window
     => XConfig l -> IO (XConfig (ModifiedLayout AvoidStruts l))
dzen conf = dzenWithFlags flags conf
 where
    fg      = "'#a8a3f7'" -- n.b quoting
    bg      = "'#3f3c6d'"
    flags   = "-e 'onstart=lower' -dock -w 400 -ta l -fg " ++ fg ++ " -bg " ++ bg


-- | Run xmonad with a xmobar status bar set to some nice defaults.
--
-- > main = xmonad =<< xmobar myConfig
-- >
-- > myConfig = def { ... }
--
-- This works pretty much the same as 'dzen' function above.
--
xmobar :: LayoutClass l Window
       => XConfig l -> IO (XConfig (ModifiedLayout AvoidStruts l))
xmobar conf = statusBar "xmobar" xmobarPP toggleStrutsKey conf

-- | Modifies the given base configuration to launch the given status bar,
-- send status information to that bar, and allocate space on the screen edges
-- for the bar.
statusBar :: LayoutClass l Window
          => String    -- ^ the command line to launch the status bar
          -> PP        -- ^ the pretty printing options
          -> (XConfig Layout -> (KeyMask, KeySym))
                       -- ^ the desired key binding to toggle bar visibility
          -> XConfig l -- ^ the base config
          -> IO (XConfig (ModifiedLayout AvoidStruts l))
statusBar cmd pp = statusBar' cmd (return pp)

-- | Like 'statusBar' with the pretty printing options embedded in the
-- X monad. The X PP value is re-executed every time the 'logHook' runs.
-- Useful if printing options need to be modified dynamically.
statusBar' :: LayoutClass l Window
           => String       -- ^ the command line to launch the status bar
           -> X PP         -- ^ the pretty printing options
           -> (XConfig Layout -> (KeyMask, KeySym))
                           -- ^ the desired key binding to toggle bar visibility
           -> XConfig l    -- ^ the base config
           -> IO (XConfig (ModifiedLayout AvoidStruts l))
statusBar' cmd xpp k conf = do
    h <- spawnPipe cmd
    return $ docks $ conf
        { layoutHook = avoidStruts (layoutHook conf)
        , logHook = do
                        logHook conf
                        pp <- xpp
                        dynamicLogWithPP pp { ppOutput = hPutStrLn h }
        , keys       = liftA2 M.union keys' (keys conf)
        }
 where
    keys' = (`M.singleton` sendMessage ToggleStruts) . k

-- | Write a string to a property on the root window.  This property is of
-- type UTF8_STRING. The string must have been processed by encodeString
-- (dynamicLogString does this).
xmonadPropLog' :: String -> String -> X ()
xmonadPropLog' prop msg = do
    d <- asks display
    r <- asks theRoot
    xlog <- getAtom prop
    ustring <- getAtom "UTF8_STRING"
    io $ changeProperty8 d r xlog ustring propModeReplace (encodeCChar msg)
 where
    encodeCChar :: String -> [CChar]
    encodeCChar = map (fromIntegral . ord)

-- | Write a string to the _XMONAD_LOG property on the root window.
xmonadPropLog :: String -> X ()
xmonadPropLog = xmonadPropLog' "_XMONAD_LOG"

-- |
-- Helper function which provides ToggleStruts keybinding
--
toggleStrutsKey :: XConfig t -> (KeyMask, KeySym)
toggleStrutsKey XConfig{modMask = modm} = (modm, xK_b )

------------------------------------------------------------------------

-- | An example log hook, which prints status information to stdout in
-- the default format:
--
-- > 1 2 [3] 4 7 : full : title
--
-- That is, the currently populated workspaces, the current
-- workspace layout, and the title of the focused window.
--
-- To customize the output format, see 'dynamicLogWithPP'.
--
dynamicLog :: X ()
dynamicLog = dynamicLogWithPP def

-- | Format the current status using the supplied pretty-printing format,
--   and write it to stdout.
dynamicLogWithPP :: PP -> X ()
dynamicLogWithPP pp = dynamicLogString pp >>= io . ppOutput pp

-- | The same as 'dynamicLogWithPP', except it simply returns the status
--   as a formatted string without actually printing it to stdout, to
--   allow for further processing, or use in some application other than
--   a status bar.
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

    -- run extra loggers, ignoring any that generate errors.
    extras <- mapM (flip catchX (return Nothing)) $ ppExtras pp

    return $ encodeString . sepBy (ppSep pp) . ppOrder pp $
                        [ ws
                        , ppLayout pp ld
                        , ppTitle  pp $ ppTitleSanitize pp wt
                        ]
                        ++ catMaybes extras

-- | Format the workspace information, given a workspace sorting function,
--   a list of urgent windows, a pretty-printer format, and the current
--   WindowSet.
pprWindowSet :: WorkspaceSort -> [Window] -> PP -> WindowSet -> String
pprWindowSet sort' urgents pp s = sepBy (ppWsSep pp) . map fmt . sort' $
            map S.workspace (S.current s : S.visible s) ++ S.hidden s
   where this     = S.currentTag s
         visibles = map (S.tag . S.workspace) (S.visible s)

         fmt w = printer pp (S.tag w)
          where printer | any (\x -> maybe False (== S.tag w) (S.findTag x s)) urgents  = ppUrgent
                        | S.tag w == this                                               = ppCurrent
                        | S.tag w `elem` visibles && isJust (S.stack w)                 = ppVisible
                        | S.tag w `elem` visibles                                       = liftA2 fromMaybe ppVisible ppVisibleNoWindows
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
-- At the present time, the current layout and window title
-- are not shown.  The xinerama workspace format shown above can be (mostly) replicated
-- using 'dynamicLogWithPP' by setting 'ppSort' to /getSortByXineramaRule/ from
-- "XMonad.Util.WorkspaceCompare".  For example,
--
-- > def { ppCurrent = dzenColor "red" "#efebe7"
-- >     , ppVisible = wrap "[" "]"
-- >     , ppSort    = getSortByXineramaRule
-- >     }
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

-- | Trim leading and trailing whitespace from a string.
trim :: String -> String
trim = f . f
    where f = reverse . dropWhile isSpace

-- | Limit a string to a certain length, adding "..." if truncated.
shorten :: Int -> String -> String
shorten = shorten' "..."

-- | Limit a string to a certain length, adding @end@ if truncated.
shorten' :: String -> Int -> String -> String
shorten' end n xs | length xs < n = xs
                  | otherwise     = take (n - length end) xs ++ end

-- | Like 'shorten', but truncate from the left instead of right.
shortenLeft :: Int -> String -> String
shortenLeft = shortenLeft' "..."

-- | Like 'shorten'', but truncate from the left instead of right.
shortenLeft' :: String -> Int -> String -> String
shortenLeft' end n xs | l < n     = xs
                      | otherwise = end ++ (drop (l - n + length end) xs)
 where l = length xs

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

-- | Strip dzen formatting or commands.
dzenStrip :: String -> String
dzenStrip = strip [] where
    strip keep x
      | null x              = keep
      | "^^" `isPrefixOf` x = strip (keep ++ "^") (drop 2 x)
      | '^' == head x       = strip keep (drop 1 . dropWhile (/= ')') $ x)
      | otherwise           = let (good,x') = span (/= '^') x
                              in strip (keep ++ good) x'

-- | Use xmobar escape codes to output a string with given foreground
--   and background colors.
xmobarColor :: String  -- ^ foreground color: a color name, or #rrggbb format
            -> String  -- ^ background color
            -> String  -- ^ output string
            -> String
xmobarColor fg bg = wrap t "</fc>"
 where t = concat ["<fc=", fg, if null bg then "" else "," ++ bg, ">"]

-- | Encapsulate text with an action. The text will be displayed, and the
-- action executed when the displayed text is clicked. Illegal input is not
-- filtered, allowing xmobar to display any parse errors. Uses xmobar's new
-- syntax wherein the command is surrounded by backticks.
xmobarAction :: String
                -- ^ Command. Use of backticks (`) will cause a parse error.
             -> String
                -- ^ Buttons 1-5, such as "145". Other characters will cause a
                -- parse error.
             -> String
                -- ^ Displayed/wrapped text.
             -> String
xmobarAction command button = wrap l r
    where
        l = "<action=`" ++ command ++ "` button=" ++ button ++ ">"
        r = "</action>"

-- | Use xmobar box to add a border to an arbitrary string.
xmobarBorder :: String -- ^ Border type. Possible values: VBoth, HBoth, Full,
                       -- Top, Bottom, Left or Right
             -> String -- ^ color: a color name, or #rrggbb format
             -> Int    -- ^ width in pixels
             -> String -- ^ output string
             -> String
xmobarBorder border color width = wrap prefix "</box>"
  where
    prefix = "<box type=" ++ border ++ " width=" ++ show width ++ " color="
      ++ color ++ ">"

-- | Encapsulate arbitrary text for display only, i.e. untrusted content if
-- wrapped (perhaps from window titles) will be displayed only, with all tags
-- ignored. Introduced in xmobar 0.21; see their documentation. Be careful not
-- to shorten the result.
xmobarRaw :: String -> String
xmobarRaw "" = ""
xmobarRaw s  = concat ["<raw=", show $ length s, ":", s, "/>"]

-- ??? add an xmobarEscape function?

-- | Strip xmobar markup, specifically the <fc>, <icon> and <action> tags and
-- the matching tags like </fc>.
xmobarStrip :: String -> String
xmobarStrip = converge (xmobarStripTags ["fc","icon","action"]) where

converge :: (Eq a) => (a -> a) -> a -> a
converge f a = let xs = iterate f a
    in fst $ head $ dropWhile (uncurry (/=)) $ zip xs $ tail xs

xmobarStripTags :: [String] -- ^ tags
        -> String -> String -- ^ with all <tag>...</tag> removed
xmobarStripTags tags = strip [] where
    strip keep [] = keep
    strip keep x
        | rest: _ <- mapMaybe dropTag tags = strip keep rest


        | '<':xs <- x = strip (keep ++ "<") xs
        | (good,x') <- span (/= '<') x = strip (keep ++ good) x' -- this is n^2 bad... but titles have few tags
      where dropTag :: String -> Maybe String
            dropTag tag = msum [fmap dropTilClose (openTag tag `stripPrefix` x),
                                                   closeTag tag `stripPrefix` x]

    dropTilClose, openTag, closeTag :: String -> String
    dropTilClose = drop 1 . dropWhile (/= '>')
    openTag str = "<" ++ str ++ "="
    closeTag str = "</" ++ str ++ ">"

-- | Transforms a pretty-printer into one not displaying the given workspaces.
--
-- For example, filtering out the @NSP@ workspace before giving the 'PP' to
-- 'dynamicLogWithPP':
--
-- > logHook = dynamicLogWithPP . filterOutWsPP [scratchpadWorkspaceTag] $ def
--
-- Here is another example, when using "XMonad.Layout.IndependentScreens".  If
-- you have handles @hLeft@ and @hRight@ for bars on the left and right screens,
-- respectively, and @pp@ is a pretty-printer function that takes a handle, you
-- could write
--
-- > logHook = let log screen handle = dynamicLogWithPP . filterOutWsPP [scratchpadWorkspaceTag] . marshallPP screen . pp $ handle
-- >           in log 0 hLeft >> log 1 hRight
filterOutWsPP :: [WorkspaceId] -> PP -> PP
filterOutWsPP ws pp = pp { ppSort = (. filterOutWs ws) <$> ppSort pp }

-- | The 'PP' type allows the user to customize the formatting of
--   status information.
data PP = PP { ppCurrent :: WorkspaceId -> String
               -- ^ how to print the tag of the currently focused
               -- workspace
             , ppVisible :: WorkspaceId -> String
               -- ^ how to print tags of visible but not focused
               -- workspaces (xinerama only)
             , ppHidden  :: WorkspaceId -> String
               -- ^ how to print tags of hidden workspaces which
               -- contain windows
             , ppHiddenNoWindows :: WorkspaceId -> String
               -- ^ how to print tags of empty hidden workspaces
             , ppVisibleNoWindows :: Maybe (WorkspaceId -> String)
               -- ^ how to print tags of empty visible workspaces
             , ppUrgent :: WorkspaceId -> String
               -- ^ format to be applied to tags of urgent workspaces.
             , ppSep :: String
               -- ^ separator to use between different log sections
               -- (window name, layout, workspaces)
             , ppWsSep :: String
               -- ^ separator to use between workspace tags
             , ppTitle :: String -> String
               -- ^ window title format
             , ppTitleSanitize :: String -> String
              -- ^  escape / sanitizes input to 'ppTitle'
             , ppLayout :: String -> String
               -- ^ layout name format
             , ppOrder :: [String] -> [String]
               -- ^ how to order the different log sections. By
               --   default, this function receives a list with three
               --   formatted strings, representing the workspaces,
               --   the layout, and the current window title,
               --   respectively. If you have specified any extra
               --   loggers in 'ppExtras', their output will also be
               --   appended to the list.  To get them in the reverse
               --   order, you can just use @ppOrder = reverse@.  If
               --   you don't want to display the current layout, you
               --   could use something like @ppOrder = \\(ws:_:t:_) ->
               --   [ws,t]@, and so on.
             , ppSort :: X ([WindowSpace] -> [WindowSpace])
               -- ^ how to sort the workspaces.  See
               -- "XMonad.Util.WorkspaceCompare" for some useful
               -- sorts.
             , ppExtras :: [X (Maybe String)]
               -- ^ loggers for generating extra information such as
               -- time and date, system load, battery status, and so
               -- on.  See "XMonad.Util.Loggers" for examples, or create
               -- your own!
             , ppOutput :: String -> IO ()
               -- ^ applied to the entire formatted string in order to
               -- output it.  Can be used to specify an alternative
               -- output method (e.g. write to a pipe instead of
               -- stdout), and\/or to perform some last-minute
               -- formatting.
             }

-- | The default pretty printing options, as seen in 'dynamicLog'.
instance Default PP where
    def   = PP { ppCurrent         = wrap "[" "]"
               , ppVisible         = wrap "<" ">"
               , ppHidden          = id
               , ppHiddenNoWindows = const ""
               , ppVisibleNoWindows= Nothing
               , ppUrgent          = id
               , ppSep             = " : "
               , ppWsSep           = " "
               , ppTitle           = shorten 80
               , ppTitleSanitize   = xmobarStrip . dzenEscape
               , ppLayout          = id
               , ppOrder           = id
               , ppOutput          = putStrLn
               , ppSort            = getSortByIndex
               , ppExtras          = []
               }

-- | Settings to emulate dwm's statusbar, dzen only.
dzenPP :: PP
dzenPP = def { ppCurrent  = dzenColor "white" "#2b4f98" . pad
                   , ppVisible  = dzenColor "black" "#999999" . pad
                   , ppHidden   = dzenColor "black" "#cccccc" . pad
                   , ppHiddenNoWindows = const ""
                   , ppUrgent   = dzenColor "red" "yellow" . pad
                   , ppWsSep    = ""
                   , ppSep      = ""
                   , ppLayout   = dzenColor "black" "#cccccc" .
                                  (\ x -> pad $ case x of
                                            "TilePrime Horizontal" -> "TTT"
                                            "TilePrime Vertical"   -> "[]="
                                            "Hinted Full"          -> "[ ]"
                                            _                      -> x
                                  )
                   , ppTitle    = ("^bg(#324c80) " ++) . dzenEscape
                   }

-- | Some nice xmobar defaults.
xmobarPP :: PP
xmobarPP = def { ppCurrent = xmobarColor "yellow" "" . wrap "[" "]"
                     , ppTitle   = xmobarColor "green"  "" . shorten 40
                     , ppVisible = wrap "(" ")"
                     , ppUrgent  = xmobarColor "red" "yellow"
                     }

-- | The options that sjanssen likes to use with xmobar, as an
-- example.  Note the use of 'xmobarColor' and the record update on
-- 'def'.
sjanssenPP :: PP
sjanssenPP = def { ppCurrent = xmobarColor "white" "black"
                 , ppTitle = xmobarColor "#00ee00" "" . shorten 120
                 }

-- | The options that byorgey likes to use with dzen, as another example.
byorgeyPP :: PP
byorgeyPP = def { ppHiddenNoWindows = showNamedWorkspaces
                , ppHidden  = dzenColor "black"  "#a8a3f7" . pad
                , ppCurrent = dzenColor "yellow" "#a8a3f7" . pad
                , ppUrgent  = dzenColor "red"    "yellow"  . pad
                , ppSep     = " | "
                , ppWsSep   = ""
                , ppTitle   = shorten 70
                , ppOrder   = reverse
                }
  where showNamedWorkspaces wsId = if any (`elem` wsId) ['a'..'z']
                                       then pad wsId
                                       else ""
