{-# LANGUAGE FlexibleContexts, PatternGuards, TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
    xmobarProp,
    xmobar,
    statusBarProp,
    statusBarPropTo,
    statusBar,
    statusBar',
    dzen,
    dzenWithFlags,
    dynamicLog,
    dynamicLogXinerama,

    xmonadPropLog,
    xmonadPropLog',
    xmonadDefProp,
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
    pprWindowSetXinerama,

    -- * Specialized spawning and cleaning
    spawnStatusBarAndRemember,
    cleanupStatusBars,

    -- * Further customization
    -- $furthercustomization
    statusBarPropConfig,
    statusBarPropToConfig,
    statusBarHandleConfig,
    statusBarHandleConfig',
    makeStatusBar,
    makeStatusBar',
    StatusBarConfig(..),

    -- * Multiple loggers
    -- $multiple

    -- * To Do
    -- $todo

  ) where

-- Useful imports

import Codec.Binary.UTF8.String (encodeString)
import Control.Applicative (liftA2)
import Control.Exception (SomeException, try)
import Control.Monad (msum, void)
import Data.Bool (bool)
import Data.Char (isSpace, ord)
import Data.List (intercalate, isPrefixOf, sortOn, stripPrefix)
import Data.Maybe (catMaybes, fromMaybe, isJust, mapMaybe)
import System.Posix.Signals (sigTERM, signalProcessGroup)
import System.Posix.Types (ProcessID)

import qualified Data.Map        as M
import qualified XMonad.StackSet as S

import Foreign.C (CChar)

import XMonad

import XMonad.Util.NamedWindows
import XMonad.Util.Run
import XMonad.Util.WorkspaceCompare
import qualified XMonad.Util.ExtensibleState as XS

import XMonad.Layout.LayoutModifier
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.ManageDocks

-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- >    import XMonad
-- >    import XMonad.Hooks.DynamicLog
--
-- The recommended way to use this module with xmobar, as well as any other
-- status bar that supports property logging (you can read more about X11
-- properties
-- [here](https://en.wikipedia.org/wiki/X_Window_System_core_protocol#Properties)
-- or
-- [here](https://tronche.com/gui/x/xlib/window-information/properties-and-atoms.html),
-- although you don't have to understand them in order to use the functions
-- below), is to use one of the following two functions:
--
--   * 'xmobarProp' if you want to use the predefined pretty-printing function
--     and toggle struts key (@mod-b@).
--
--   * 'statusBarProp' if you want to define these things yourself.
--
-- These functions are preferred over the other options listed below, as they
-- take care of all the necessary plumbing—no shell scripting required!
--
-- For example, to use the 'xmobarProp' function you could do
--
-- > main = xmonad =<< xmobarProp myConfig
-- >
-- > myConfig = def { ... }
--
-- With 'statusBarProp', this would look something like the following
--
-- > main = xmonad =<< statusBarProp "xmobar" myXmobarPP myToggleStrutsKey myConfig
-- >
-- > myXmobarPP = def { ... }
-- > toggleStrutsKey XConfig{ modMask = modm } = (modm, xK_b)
-- > myConfig = def { ... }
--
-- You then have to tell your status bar to read from the @_XMONAD_LOG@ property
-- of the root window.  In the case of xmobar, this is simply achieved by using
-- the @XMonadLog@ plugin instead of @StdinReader@ in your @.xmobarrc@:
--
-- > Config { ...
-- >        , commands = [ Run XMonadLog, ... ]
-- >        , template = "%XMonadLog% }{ ..."
-- >        }
--
-- If you don't have an @.xmobarrc@, create it; the @XMonadLog@ plugin is not
-- part of the default xmobar configuration and your status bar will not show
-- otherwise!
--
-- Because 'statusBarProp' lets you define your own executable, you can also
-- give it a different status bar entirely; you only need to make sure that the
-- status bar supports reading a property string from the root window, or use
-- a wrapper that reads the property and pipes it into the bar (e.g.
-- @xmonadpropread | dzen2@, see @scripts/xmonadpropread.hs@).
--
-- If you don't want to use the default property, you can specify your own
-- with the 'statusBarPropTo' function.
--
-- If your status bar does not support property-based logging, you may also try
-- 'statusBar' and 'statusBar'', as well as the `xmobar` function, instead.
-- They can be used in the same way as the 'statusBarProp' and 'xmobarProp'
-- functions above (for xmobar, you will now use the @StdinReader@
-- plugin in your @.xmobarrc@).  Instead of writing to a property, these
-- functions open a pipe and make the given status bar read from that pipe.
-- Please be aware that this kind of setup is very bug-prone and hence is
-- discouraged: if anything goes wrong with the bar, xmonad will freeze.
--
-- If you do not want to use any of the "batteries included" functions above,
-- you can also add all of the necessary plumbing yourself (the definition of
-- 'statusBarProp' may still come in handy here).
--
-- 'xmonadPropLog' allows you to write a string to the @_XMONAD_LOG@ property of
-- the root window.  Together with 'dynamicLogString', you can now simply set
-- your logHook to the appropriate function, for instance
--
-- > main = xmonad $ def {
-- >    ...
-- >    , logHook = xmonadPropLog =<< dynamicLogString myPP
-- >    ...
-- >    }
--
-- If you want to define your own property name, use 'xmonadPropLog'' instead of
-- 'xmonadPropLog'.
--
-- If you just want to use the default pretty-printing format, you can replace
-- @myPP@ with @def@ in the above logHook.
--
-- Note that setting the @logHook@ only sets up xmonad's output; you are
-- responsible for starting your own status bar program and making sure it reads
-- from the property that xmonad writes to.  To start your bar, simply put it
-- into your 'startupHook'.  You will also have also have to add 'docks' and
-- 'avoidStruts' to your config.  Putting all of this together would look
-- something like
--
-- > import XMonad.Util.SpawnOnce (spawnOnce)
-- > import XMonad.Hooks.ManageDocks (avoidStruts, docks)
-- >
-- > main = do
-- >     xmonad $ docks $ def {
-- >       ...
-- >       , logHook     = xmonadPropLog =<< dynamicLogString myPP
-- >       , startupHook = spawnOnce "xmobar"
-- >       , layoutHook  = avoidStruts myLayout
-- >       ...
-- >       }
-- > myPP = def { ... }
-- > myLayout = ...
--
-- If you want a keybinding to toggle your bar, you will also need to add this
-- to the rest of your keybindings.
--
-- The above has the problem that xmobar will not get restarted whenever you
-- restart xmonad ('XMonad.Util.SpawnOnce.spawnOnce' will simply prevent your
-- chosen status bar from spawning again). Using 'statusBarProp', however, takes
-- care of the necessary plumbing /and/ keeps track of the started status bars, so
-- they can be correctly restarted with xmonad. This is achieved using
-- 'spawnStatusBarAndRemember' to start them and 'cleanupStatusBars' to kill
-- previously started ones.
--
-- Even if you don't use a statusbar, you can still use 'dynamicLogString' to
-- show on-screen notifications in response to some events. For example, to show
-- the current layout when it changes, you could make a keybinding to cycle the
-- layout and display the current status:
--
-- > ((mod1Mask, xK_a), sendMessage NextLayout >> (dynamicLogString myPP >>= \d->spawn $"xmessage "++d))
--
-- If you use a status bar that does not support reading from a property log
-- (like dzen), and you don't want to use the 'statusBar' or 'statusBar''
-- functions, you can, again, also manually add all of the required components.
--
-- This works much like the property based solution above, just that you will
-- want to use 'dynamicLog' or 'dynamicLogXinerama' in place of 'xmonadPropLog'.
--
-- > main = xmonad $ def {
-- >    ...
-- >    , logHook = dynamicLog
-- >    ...
-- >    }
--
-- For more flexibility, you can also use 'dynamicLogWithPP' and supply your own
-- pretty-printing format (by either defining one from scratch, or customizing
-- one of the provided examples).  For example:
--
-- >    -- use sjanssen's pretty-printer format, but with the sections
-- >    -- in reverse
-- >    logHook = dynamicLogWithPP $ sjanssenPP { ppOrder = reverse }
--
-- Again, you will have to do all the necessary plumbing yourself.  In addition,
-- you are also responsible for creating a pipe for you status bar to read from:
--
-- > import XMonad.Util.Run (hPutStrLn, spawnPipe)
-- >
-- > main = do
-- >     h <- spawnPipe "dzen2 -options -foo -bar"
-- >     xmonad $ def {
-- >       ...
-- >       , logHook = dynamicLogWithPP $ def { ppOutput = hPutStrLn h }
-- >       ...
-- >       }
--
-- In the above, note that if you use @spawnPipe@ you need to redefine the
-- 'ppOutput' field of your pretty-printer, as was done in the example above; by
-- default the status will be printed to stdout rather than the pipe you create.
--

-- $todo
--
--   * incorporate dynamicLogXinerama into the PP framework somehow

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
-- This function works much in the same way as the 'dzen' function, only that it
-- can also be used to customize the arguments passed to dzen2, e.g changing the
-- default width and height of dzen2.
--
-- You should use this function only when the default 'dzen' function does not
-- serve your purpose.
--
dzenWithFlags :: LayoutClass l Window
              => String     -- ^ Flags to give to @dzen@
              -> XConfig l  -- ^ The base config
              -> IO (XConfig (ModifiedLayout AvoidStruts l))
dzenWithFlags flags = statusBar ("dzen2 " ++ flags) dzenPP toggleStrutsKey

-- | Run xmonad with a dzen status bar set to some nice defaults.
--
-- > main = xmonad =<< dzen myConfig
-- >
-- > myConfig = def { ... }
--
-- This works pretty much the same as the 'xmobar' function.
--
dzen :: LayoutClass l Window
     => XConfig l  -- ^ The base config
     -> IO (XConfig (ModifiedLayout AvoidStruts l))
dzen = dzenWithFlags flags
 where
    fg      = "'#a8a3f7'" -- n.b quoting
    bg      = "'#3f3c6d'"
    flags   = "-e 'onstart=lower' -dock -w 400 -ta l -fg " ++ fg ++ " -bg " ++ bg

-- | Run xmonad with a property-based xmobar status bar set to some nice
-- defaults.
--
-- > main = xmonad =<< xmobarProp myConfig
-- >
-- > myConfig = def { ... }
--
-- The intent is that the above config file should provide a nice
-- status bar with minimal effort. Note that you still need to configure
-- xmobar to use the @XMonadLog@ plugin instead of the default @StdinReader@,
-- see above.
--
-- If you wish to customize the status bar format at all, you'll have to
-- use the 'statusBarProp' function instead.
--
-- The binding uses the "XMonad.Hooks.ManageDocks" module to automatically
-- handle screen placement for xmobar, and enables 'mod-b' for toggling
-- the menu bar.
--
xmobarProp :: LayoutClass l Window
           => XConfig l  -- ^ The base config
           -> IO (XConfig (ModifiedLayout AvoidStruts l))
xmobarProp = statusBarProp "xmobar" xmobarPP toggleStrutsKey

-- | This function works like 'xmobarProp', but uses pipes instead of
-- property-based logging.
xmobar :: LayoutClass l Window
       => XConfig l  -- ^ The base config
       -> IO (XConfig (ModifiedLayout AvoidStruts l))
xmobar = statusBar "xmobar" xmobarPP toggleStrutsKey

-- | Modifies the given base configuration to launch the given status bar, write
-- status information to the @_XMONAD_LOG@ property for the bar to read, and
-- allocate space on the screen edges for the bar.
statusBarProp :: LayoutClass l Window
              => String    -- ^ The command line to launch the status bar
              -> PP        -- ^ The pretty printing options
              -> (XConfig Layout -> (KeyMask, KeySym))
                           -- ^ The desired key binding to toggle bar visibility
              -> XConfig l -- ^ The base config
              -> IO (XConfig (ModifiedLayout AvoidStruts l))
statusBarProp = statusBarPropTo xmonadDefProp

-- | Like 'statusBarProp', but one is able to specify the property to be written
-- to.
statusBarPropTo :: LayoutClass l Window
                => String    -- ^ Property to write the string to
                -> String    -- ^ The command line to launch the status bar
                -> PP        -- ^ The pretty printing options
                -> (XConfig Layout -> (KeyMask, KeySym))
                             -- ^ The desired key binding to toggle bar visibility
                -> XConfig l -- ^ The base config
                -> IO (XConfig (ModifiedLayout AvoidStruts l))
statusBarPropTo prop a p k c = do {sb <- statusBarPropToConfig prop a p; makeStatusBar' sb k c}


-- | Spawns a status bar and saves its PID. This is useful when the status bars
-- should be restarted with xmonad. Use this in combination with 'cleanupStatusBars'.
--
-- Note: in some systems, multiple processes might start, even though one command is
-- provided. This means the first PID, of the group leader, is saved.
--
spawnStatusBarAndRemember :: String -- ^ The command used to spawn the status bar
                          -> X ()
spawnStatusBarAndRemember cmd = do
  newPid <- spawnPID cmd
  XS.modify (StatusBarPIDs . (newPid :) . getPIDs)

-- This newtype wrapper, together with the ExtensionClass instance makes use of
-- the extensible state to save the PIDs bewteen xmonad restarts.
newtype StatusBarPIDs = StatusBarPIDs { getPIDs :: [ProcessID] }
  deriving (Show, Read)

instance ExtensionClass StatusBarPIDs where
  initialValue = StatusBarPIDs []
  extensionType = PersistentExtension


-- | Kills the status bars started with 'spawnStatusBarAndRemember', and resets
-- the state. This could go for example at the beginning of the startupHook.
--
-- Concretely, this function sends a 'sigTERM' to the saved PIDs using
-- 'signalProcessGroup' to effectively terminate all processes, regardless
-- of how many were started by using  'spawnStatusBarAndRemember'.
--
-- There is one caveat to keep in mind: to keep the implementation simple;
-- no checks are executed before terminating the processes. This means: if the
-- started process dies for some reason, and enough time passes for the PIDs
-- to wrap around, this function might terminate another process that happens
-- to have the same PID. However, this isn't a typical usage scenario.
--
cleanupStatusBars :: X ()
cleanupStatusBars =
    getPIDs <$> XS.get
    >>= (io . mapM_ killPid)
    >> XS.put (StatusBarPIDs [])
   where
    killPid :: ProcessID -> IO ()
    killPid pidToKill = void $ try @SomeException (signalProcessGroup sigTERM pidToKill)


-- | Like 'statusBarProp', but uses pipes instead of property-based logging.
-- Only use this function if your status bar does not support reading from a
-- property of the root window.
statusBar :: LayoutClass l Window
          => String    -- ^ The command line to launch the status bar
          -> PP        -- ^ The pretty printing options
          -> (XConfig Layout -> (KeyMask, KeySym))
                       -- ^ The desired key binding to toggle bar visibility
          -> XConfig l -- ^ The base config
          -> IO (XConfig (ModifiedLayout AvoidStruts l))
statusBar = makeSB .: statusBarHandleConfig


-- | Like 'statusBar' with the pretty printing options embedded in the
-- 'X' monad. The @X PP@ value is re-executed every time the logHook runs.
-- Useful if printing options need to be modified dynamically.
statusBar' :: LayoutClass l Window
           => String       -- ^ The command line to launch the status bar
           -> X PP         -- ^ The pretty printing options
           -> (XConfig Layout -> (KeyMask, KeySym))
                           -- ^ The desired key binding to toggle bar visibility
           -> XConfig l    -- ^ The base config
           -> IO (XConfig (ModifiedLayout AvoidStruts l))
statusBar' = makeSB .: statusBarHandleConfig'

-- | Incorporates a 'StatusBarConfig' into an 'XConfig' by taking care of the
-- necessary plumbing (starting, restarting and logging to it).
--
-- Using this function multiple times to combine status bars may result in
-- only one status bar working properly. See the section on using multiple
-- status bars for more details.
makeStatusBar :: LayoutClass l Window
              => StatusBarConfig -- ^ The status bar config
              -> XConfig l          -- ^ The base config
              -> IO (XConfig l)
makeStatusBar (StatusBarConfig lh sh ch) conf =
  return $ conf
    { logHook     = logHook conf *> lh
    , startupHook = startupHook conf *> ch *> sh
    }

-- | Like 'makeStatusBar', but takes an extra key to toggle struts. It also
-- applies the 'avoidStruts' layout modifier and the 'docks' combinator.
--
-- Using this function multiple times to combine status bars may result in
-- only one status bar working properly. See the section on using multiple
-- status bars for more details.
makeStatusBar' :: LayoutClass l Window
               => StatusBarConfig -- ^ The status bar config
               -> (XConfig Layout -> (KeyMask, KeySym))
                                    -- ^ The key binding
               -> XConfig l          -- ^ The base config
               -> IO (XConfig (ModifiedLayout AvoidStruts l))
makeStatusBar' sb k conf = do
  conf' <- makeStatusBar sb conf
  return $ docks $ conf' { layoutHook = avoidStruts (layoutHook conf')
                         , keys       = (<>) <$> keys' <*> keys conf'
                         }
  where keys' = (`M.singleton` sendMessage ToggleStruts) . k

-- | A helper function to be used with 'statusBar', 'statusBarProp'
-- and their variants.
makeSB :: LayoutClass l Window
       => IO StatusBarConfig -- ^ The status bar config
       -> (XConfig Layout -> (KeyMask, KeySym))
                            -- ^ The key binding
       -> XConfig l          -- ^ The base config
       -> IO (XConfig (ModifiedLayout AvoidStruts l))
makeSB sb k conf = do
  sb' <- sb
  makeStatusBar' sb' k conf

-- | Write a string to a property on the root window.  This property is of type
-- @UTF8_STRING@. The string must have been processed by 'encodeString'
-- ('dynamicLogString' does this).
xmonadPropLog' :: String  -- ^ Property name
               -> String  -- ^ Message to be written to the property
               -> X ()
xmonadPropLog' prop msg = do
    d <- asks display
    r <- asks theRoot
    xlog <- getAtom prop
    ustring <- getAtom "UTF8_STRING"
    io $ changeProperty8 d r xlog ustring propModeReplace (encodeCChar msg)
 where
    encodeCChar :: String -> [CChar]
    encodeCChar = map (fromIntegral . ord)

-- | Write a string to the @_XMONAD_LOG@ property on the root window.
xmonadPropLog :: String -> X ()
xmonadPropLog = xmonadPropLog' xmonadDefProp

-- |
-- Helper function which provides ToggleStruts keybinding
--
toggleStrutsKey :: XConfig t -> (KeyMask, KeySym)
toggleStrutsKey XConfig{modMask = modm} = (modm, xK_b )

-- | The default property xmonad writes to. (@_XMONAD_LOG@).
xmonadDefProp :: String
xmonadDefProp = "_XMONAD_LOG"

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

    -- window titles
    let stack  = S.index winset
        focWin = S.peek  winset
        ppWin :: Window -> String -> String  -- pretty print a window title
            = bool (ppTitleUnfocused pp) (ppTitle pp) . (focWin ==) . Just
    winNames <- traverse (fmap show . getName) stack
    let ppNames = unwords . filter (not . null) $
            zipWith (\w n -> ppWin w $ ppTitleSanitize pp n) stack winNames

    -- run extra loggers, ignoring any that generate errors.
    extras <- mapM (`catchX` return Nothing) $ ppExtras pp

    return $ encodeString . sepBy (ppSep pp) . ppOrder pp $
                        [ ws
                        , ppLayout pp ld
                        , ppNames
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
          where printer | any (\x -> (== Just (S.tag w)) (S.findTag x s)) urgents  = ppUrgent
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
                        . sortOn S.screen $ S.current ws : S.visible ws
        offscreen = map S.tag . filter (isJust . S.stack)
                        . sortOn S.tag $ S.hidden ws

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
                      | otherwise = end ++ drop (l - n + length end) xs
 where l = length xs

-- | Output a list of strings, ignoring empty ones and separating the
--   rest with the given separator.
sepBy :: String   -- ^ separator
      -> [String] -- ^ fields to output
      -> String
sepBy sep = intercalate sep . filter (not . null)

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

-- | Strip xmobar markup, specifically the <fc>, <icon> and <action> tags and
-- the matching tags like </fc>.
xmobarStrip :: String -> String
xmobarStrip = converge (xmobarStripTags ["fc","icon","action"])

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


-- $furthercustomization
-- Internally, the status bars are managed through the 'StatusBarConfig', which provides a convenient
-- abstraction over what a status bar is and how to manage it.
-- Instead of directly modifying the config, the functions 'statusBarProp' and 'statusBar' create an
-- appropriate 'StatusBarConfig' and call 'makeStatusBar' or 'makeStatusBar'' on it. This enables the user
-- to have more flexibility and control over the status bars, without having do the plumbing themselves.
--
-- The difference between 'makeStatusBar' and 'makeStatusBar'' is that 'makeStatusBar' tries to stay
-- out of your way, whereas 'makeStatusBar'' configures an extra keybinding to toggle the status bars,
-- and also applies the 'avoidStruts' layout modifier as well as the 'docks' combinator, which makes it
-- similar to the 'statusBar' function and its variants.
-- Using this interface has the benefit of enabling a cleaner config and composable status bars.
--
-- For example, if you want to use property logging as provided by 'statusBarProp' you could do the following:
--
-- > main = do
-- >   mySB <- statusBarPropConfig "xmobar" myPP
-- >   xmonad =<< makeStatusBar mySB myConf
--
-- This plays nicer with other combinators that you might have already in your config:
--
-- > main = do
-- >   mySB <- statusBarPropConfig "xmobar" myPP
-- >   xmonad =<< (makeStatusBar mySB . ewmh . docks $ def {...})
--
-- The different statusBar functions rely on the following status bar configs:
--
-- * 'statusBarProp' relies on 'statusBarPropConfig'
--
-- * 'statusBarPropTo' relies on 'statusBarPropConfig'
--
-- * 'statusBar' relies on 'statusBarHandleConfig'
--
-- * 'statusBar'' relies on 'statusBarHandleConfig''

-- | This datataype abstracts a status bar to provide a common interface functions like 'statusBar'
-- or 'statusBarProp'. Once defined, a status bar can be incorporated in 'XConfig' by using 'makeStatusBar',
-- which takes care of all the necessary plumbing.
data StatusBarConfig = StatusBarConfig  { sbLogHook     :: X ()
                                         -- ^ What and how to log to the status bar.
                                        , sbStartupHook :: X ()
                                        -- ^ How to start the status bar.
                                        , sbCleanupHook :: X ()
                                        -- ^ How to kill the status bar when xmonad is restarted.
                                        -- This is useful when the status bar is not started
                                        -- with a pipe.
                                        }

instance Semigroup StatusBarConfig where
    StatusBarConfig l s c <> StatusBarConfig l' s' c' =
      StatusBarConfig (l <> l') (s <> s') (c <> c')

instance Monoid StatusBarConfig where
    mempty = StatusBarConfig mempty mempty mempty

-- | Per default, all the hooks do nothing.
instance Default StatusBarConfig where
    def = mempty


-- | Creates a 'StatusBarConfig' that uses property logging to @_XMONAD_LOG@, which
-- is set in 'xmonadDefProp'
statusBarPropConfig :: String    -- ^ The command line to launch the status bar
                    -> PP        -- ^ The pretty printing options
                    -> IO StatusBarConfig
statusBarPropConfig = statusBarPropToConfig xmonadDefProp

-- | Like 'statusBarPropConfig', but lets you define the property
statusBarPropToConfig :: String    -- ^ Property to write the string to
                      -> String    -- ^ The command line to launch the status bar
                      -> PP        -- ^ The pretty printing options
                      -> IO StatusBarConfig
statusBarPropToConfig prop cmd pp = pure $ (def:: StatusBarConfig)
    { sbLogHook     = xmonadPropLog' prop =<< dynamicLogString pp
    , sbStartupHook = spawnStatusBarAndRemember cmd
    , sbCleanupHook = cleanupStatusBars
    }

-- | Creates a 'StatusBarConfig' with logging to the standard input
statusBarHandleConfig :: String       -- ^ The command line to launch the status bar
                      -> PP           -- ^ The pretty printing options
                      -> IO StatusBarConfig
statusBarHandleConfig cmd pp = do
    h <- spawnPipe cmd
    return $ def { sbLogHook = dynamicLogWithPP pp { ppOutput = hPutStrLn h } }

-- | Like 'statusBarHandleConfig', but the pretty printing options are embedded in the
-- 'X' monad.
statusBarHandleConfig' :: String       -- ^ The command line to launch the status bar
                       -> X PP         -- ^ The pretty printing options
                       -> IO StatusBarConfig
statusBarHandleConfig' cmd xpp  = do
    h <- spawnPipe cmd
    return $ def { sbLogHook = xpp >>= \pp -> dynamicLogWithPP pp { ppOutput = hPutStrLn h } }

-- $multiple
-- A pattern that is often found in a lot of configs that want multiple status bars,
-- generally goes something like this:
--
-- > main = do
-- >   xmproc0 <- spawnPipe "xmobar -x 0 $HOME/.config/xmobar/xmobarrc_top"
-- >   xmproc1 <- spawnPipe "xmobar -x 0 $HOME/.config/xmobar/xmobarrc_bottom"
-- >   xmproc2 <- spawnPipe "xmobar -x 1 $HOME/.config/xmobar/xmobarrc1"
-- >   xmonad $ def {
-- >     ...
-- >     , logHook = dynamicLogWithPP ppTop { ppOutput = hPutStrLn xmproc0 }
-- >              >> dynamicLogWithPP ppBottom { ppOutput = hPutStrLn xmproc1 }
-- >              >> dynamicLogWithPP pp1 { ppOutput = hPutStrLn xmproc2 }
-- >     ...
-- >   }
--
-- Which has a lot of boilerplate and error-prone. Using the provided functions
-- in this section, the amount of boilerplate is reduced.
-- If you wish to have multiple status bars managed by xmonad, you can do so by creating
-- the status bar configs and combining them with '<>':
--
-- > main = do
-- >   xmobarTop    <- statusBarHandleConfig "xmobar -x 0 $HOME/.config/xmobar/xmobarrc_top"    ppTop
-- >   xmobarBottom <- statusBarHandleConfig "xmobar -x 0 $HOME/.config/xmobar/xmobarrc_bottom" ppBottom
-- >   xmobar1      <- statusBarHandleConfig "xmobar -x 1 $HOME/.config/xmobar/xmobarrc1"       pp1
-- >   xmonad =<< makeStatusBar (xmobarTop <> xmobarBottom <> xmobar1) myConfig
--
-- Or if you're feeling adventurous:
--
-- > myBars = map (uncurry statusBarHandleConfig) [ ("xmobar -x 0 $HOME/.config/xmobar/xmobarrc_top",    ppTop)
-- >                                              , ("xmobar -x 0 $HOME/.config/xmobar/xmobarrc_bottom", ppBottom)
-- >                                              , ("xmobar -x 1 $HOME/.config/xmobar/xmobarrc1",       pp1) ]
-- > main = do
-- >   sbs <- sequence myBars
-- >   xmonad =<< makeStatusBar (mconcat sbs) myConfig
--
-- The above examples also work if the different status bars support different
-- logging methods: you could do mix property logging and logging via standard input.
-- One thing to keep in mind: if multiple bars read from the same property, their content
-- will be the same. If you want to use property-based logging with multiple bars,
-- they should read from different properties.

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
               -- ^ window title format for the focused window
             , ppTitleUnfocused :: String -> String
               -- ^ window title format for unfocused windows
             , ppTitleSanitize :: String -> String
              -- ^ escape / sanitizes input to 'ppTitle' and
              --   'ppTitleUnfocused'
             , ppLayout :: String -> String
               -- ^ layout name format
             , ppOrder :: [String] -> [String]
               -- ^ how to order the different log sections. By
               --   default, this function receives a list with three
               --   formatted strings, representing the workspaces,
               --   the layout, and the current window titles,
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
               , ppTitleUnfocused  = const ""
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

-- | A helper combinator
(.:) :: (d -> c) -> (a -> b -> d) -> a -> b -> c
(.:) = (.) . (.)
