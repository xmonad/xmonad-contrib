{-# LANGUAGE MultiWayIf #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Util.Loggers
-- Description :  A collection of simple logger functions and formatting utilities.
-- Copyright   :  (c) Brent Yorgey, Wirt Wolff
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  <byorgey@gmail.com>
-- Stability   :  unstable
-- Portability :  unportable
--
-- A collection of simple logger functions and formatting utilities
-- which can be used in the 'XMonad.Hooks.StatusBar.PP.ppExtras' field of
-- a pretty-printing status logger format. See "XMonad.Hooks.StatusBar.PP"
-- for more information.
-----------------------------------------------------------------------------

module XMonad.Util.Loggers (
    -- * Usage
    -- $usage

      Logger

    -- * System Loggers
    -- $system
    , aumixVolume
    , battery
    , date
    , loadAvg
    , maildirNew, maildirUnread
    , logCmd , logFileCount

    -- * XMonad Loggers
    -- $xmonad
    , logCurrent, logLayout
    , logTitle, logTitles, logTitles'
    , logClassname, logClassnames, logClassnames'
    , logConst, logDefault, (.|)
    -- * XMonad: Screen-specific Loggers
    -- $xmonad-screen
    , logCurrentOnScreen, logLayoutOnScreen
    , logTitleOnScreen, logClassnameOnScreen, logWhenActive
    , logTitlesOnScreen, logTitlesOnScreen'
    , logClassnamesOnScreen, logClassnamesOnScreen'
    , TitlesFormat(..)
    , ClassnamesFormat(..)
    -- * Formatting Utilities
    -- $format
    , onLogger
    , wrapL, fixedWidthL
    , logSp, padL
    , shortenL
    , dzenColorL, xmobarColorL

  ) where

import XMonad (Default, gets, liftIO, Window)
import XMonad.Core
import qualified XMonad.StackSet as W
import XMonad.Hooks.StatusBar.PP
import XMonad.Hooks.UrgencyHook (readUrgents)
import XMonad.Util.Font (Align (..))
import XMonad.Util.NamedWindows (getName, getNameWMClass)

import Control.Exception as E
import XMonad.Prelude (find, fromMaybe, isPrefixOf, isSuffixOf, WindowScreen)
import Data.Time (defaultTimeLocale, formatTime, getCurrentTime)
import System.Directory (getDirectoryContents)
import System.IO (hGetLine)
import System.Process (runInteractiveCommand)

econst :: Monad m => a -> IOException -> m a
econst = const . return

-- $usage
-- Use this module by importing it into your @xmonad.hs@:
--
-- > import XMonad.Util.Loggers
--
-- Then, add one or more loggers to the
-- 'XMonad.Hooks.StatusBar.PP.ppExtras' field of your
-- "XMonad.Hooks.StatusBar.PP", possibly with extra formatting .
-- For example:
--
-- > myPP = def {
-- >            ppExtras = [ padL loadAvg, logCmd "fortune -n 40 -s" ]
-- >         }
-- >   -- gives something like " 3.27 3.52 3.26 Drive defensively.  Buy a tank."
--
-- See the formatting section below for another example using
-- a @where@ block to define some formatted loggers for a top-level
-- @myPP@.
--
-- Loggers are named either for their function, as in 'battery',
-- 'aumixVolume', and 'maildirNew', or are prefixed with \"log\" when
-- making use of other functions or by analogy with the pp* functions.
-- For example, the logger version of 'XMonad.Hooks.StatusBar.PP.ppTitle'
-- is 'logTitle', and 'logFileCount' loggerizes the result of file
-- counting code.
--
-- Formatting utility names are generally as short as possible and
-- carry the suffix \"L\". For example, the logger version of
-- 'XMonad.Hooks.StatusBar.PP.shorten' is 'shortenL'.
--
-- Of course, there is nothing really special about these so-called
-- \"loggers\": they are just @X (Maybe String)@ actions.  So you can
-- use them anywhere you would use an @X (Maybe String)@, not just
-- with PP.
--
-- Additional loggers welcome!



-- | 'Logger' is just a convenient synonym for @X (Maybe String)@.
type Logger = X (Maybe String)

-- $system

-- | Get the current volume with @aumix@. <http://jpj.net/~trevor/aumix.html>
aumixVolume :: Logger
aumixVolume = logCmd "aumix -vq"

-- | Get the battery status (percent charge and charging\/discharging
--   status). This is an ugly hack and may not work for some people.
--   At some point it would be nice to make this more general\/have
--   fewer dependencies (assumes @acpi@ and @sed@ are installed.)
battery :: Logger
battery = logCmd "acpi | sed -r 's/.*?: (.*%).*/\\1/; s/[dD]ischarging, ([0-9]+%)/\\1-/; s/[cC]harging, ([0-9]+%)/\\1+/; s/[cC]harged, //'"

-- | Get the current date and time, and format them via the
--   given format string.  The format used is the same as that used
--   by the C library function strftime; for example,
--   @date \"%a %b %d\"@ might display something like @Tue Feb 19@.
--   For more information see something like
--   <http://www.cplusplus.com/reference/clibrary/ctime/strftime.html>.
date :: String -> Logger
date fmt = io $ Just . formatTime defaultTimeLocale fmt <$> getCurrentTime

-- | Get the load average.  This assumes that you have a
--   utility called @uptime@ and that you have @sed@
--   installed; these are fairly common on GNU\/Linux systems but it
--   would be nice to make this more general.
loadAvg :: Logger
loadAvg = logCmd "uptime | sed 's/.*: //; s/,//g'"

-- | Create a 'Logger' from an arbitrary shell command.
logCmd :: String -> Logger
logCmd c = io $ do (_, out, _, _) <- runInteractiveCommand c
                   fmap Just (hGetLine out) `E.catch` econst Nothing
                   -- no need to waitForProcess, we ignore SIGCHLD

-- | Get a count of filtered files in a directory.
-- See 'maildirUnread' and 'maildirNew' source for usage examples.
logFileCount :: FilePath          -- ^ directory in which to count files
             -> (String -> Bool)  -- ^ predicate to match if file should be counted
             -> Logger
logFileCount d p = do
    c <- liftIO ( getDirectoryContents d)
    let n = length $ Prelude.filter p c
    case n of
        0 -> return Nothing
        _ -> return $ Just $ show n

-- | Get a count of unread mails in a maildir. For maildir format
-- details, to write loggers for other classes of mail, see
-- <http://cr.yp.to/proto/maildir.html> and 'logFileCount'.
maildirUnread :: FilePath -> Logger
maildirUnread mdir = logFileCount (mdir ++ "/cur/") (isSuffixOf ",")

-- | Get a count of new mails in a maildir.
maildirNew :: FilePath -> Logger
maildirNew mdir = logFileCount (mdir ++ "/new/") (not . isPrefixOf ".")

-- $xmonad
--
-- A very small sample of what you can log since you have access to X. For
-- example you can loggerize the number of windows on each workspace, or
-- titles on other workspaces, or the id of the previously focused workspace....

-- | Internal function to get a wrapped title string from a window
fetchWindowTitle :: Window -> X String
fetchWindowTitle = fmap show . getName

-- | Get the title (name) of the focused window.
logTitle :: Logger
logTitle = logWindowInfoFocusedWindow fetchWindowTitle

-- | Get the titles of all windows on the visible workspace of the given
-- screen and format them according to the given functions.
--
-- ==== __Example__
--
-- > myXmobarPP :: X PP
-- > myXmobarPP = pure $ def
-- >   { ppOrder  = [ws, l, _, wins] -> [ws, l, wins]
-- >   , ppExtras = [logTitles formatFocused formatUnfocused]
-- >   }
-- >  where
-- >   formatFocused   = wrap "[" "]" . xmobarColor "#ff79c6" "" . shorten 50 . xmobarStrip
-- >   formatUnfocused = wrap "(" ")" . xmobarColor "#bd93f9" "" . shorten 30 . xmobarStrip
--
logTitlesOnScreen
  :: ScreenId           -- ^ Screen to log the titles on
  -> (String -> String) -- ^ Formatting for the focused   window
  -> (String -> String) -- ^ Formatting for the unfocused window
  -> Logger
logTitlesOnScreen sid formatFoc formatUnfoc =
  logWindowInfoOnScreen fetchWindowTitle sid formatFoc formatUnfoc formatUnfoc

-- | Like 'logTitlesOnScreen' but with support for urgent windows.  To
-- be used with "XMonad.Hooks.UrgencyHook".
logTitlesOnScreen' :: ScreenId -> TitlesFormat -> Logger
logTitlesOnScreen' sid (TitlesFormat formatFoc formatUnfoc formatUrg) =
  logWindowInfoOnScreen fetchWindowTitle sid formatFoc formatUnfoc formatUrg

-- | Like 'logTitlesOnScreen', but directly use the "focused" screen
-- (the one with the currently focused workspace).
logTitles :: (String -> String) -> (String -> String) -> Logger
logTitles formatFoc formatUnfoc =
  logWindowInfoFocusedScreen fetchWindowTitle formatFoc formatUnfoc formatUnfoc

-- | Variant of 'logTitles', but with support for urgent windows.
logTitles' :: TitlesFormat -> Logger
logTitles' (TitlesFormat formatFoc formatUnfoc formatUrg) =
  logWindowInfoFocusedScreen fetchWindowTitle formatFoc formatUnfoc formatUrg

-- | Formatting applied to the titles of certain windows.
data TitlesFormat = TitlesFormat
  { focusedFormat   :: String -> String  -- ^ Focused formatting.
  , unfocusedFormat :: String -> String  -- ^ Unfocused formatting.
  , urgentFormat    :: String -> String  -- ^ Formatting when urgent.
  }

-- | How to format these titles by default when using 'logTitles'' and
-- 'logTitlesOnScreen''.
instance Default TitlesFormat where
  def = TitlesFormat
    { focusedFormat   = xmobarFocusedFormat
    , unfocusedFormat = xmobarWsFormat
    , urgentFormat    = xmobarUrgentFormat
    }

-- | Internal function to get a wrapped classname string from a window
fetchWindowClassname :: Window -> X String
fetchWindowClassname = fmap show . getNameWMClass

-- | Get the classname of the focused window.
logClassname :: Logger
logClassname = logWindowInfoFocusedWindow fetchWindowClassname

-- | Get the classnames of all windows on the visible workspace of the given
-- screen and format them according to the given functions.
logClassnamesOnScreen
  :: ScreenId           -- ^ Screen to log the classnames on
  -> (String -> String) -- ^ Formatting for the focused window
  -> (String -> String) -- ^ Formatting for the unfocused window
  -> Logger
logClassnamesOnScreen sid formatFoc formatUnfoc =
  logWindowInfoOnScreen fetchWindowClassname sid formatFoc formatUnfoc formatUnfoc

-- | Like 'logClassnamesOnScreen' but with support for urgent windows.  To
-- be used with "XMonad.Hooks.UrgencyHook".
logClassnamesOnScreen' :: ScreenId -> ClassnamesFormat -> Logger
logClassnamesOnScreen' sid (ClassnamesFormat formatFoc formatUnfoc formatUrg) =
  logWindowInfoOnScreen fetchWindowClassname sid formatFoc formatUnfoc formatUrg

-- | Like 'logClassnamesOnScreen', but directly use the "focused" screen
-- (the one with the currently focused workspace).
logClassnames :: (String -> String) -> (String -> String) -> Logger
logClassnames formatFoc formatUnfoc =
  logWindowInfoFocusedScreen fetchWindowClassname formatFoc formatUnfoc formatUnfoc

-- | Variant of 'logClassnames', but with support for urgent windows.
logClassnames' :: ClassnamesFormat -> Logger
logClassnames' (ClassnamesFormat formatFoc formatUnfoc formatUrg) =
  logWindowInfoFocusedScreen fetchWindowClassname formatFoc formatUnfoc formatUrg

-- | Formatting applied to the classnames of certain windows.
data ClassnamesFormat = ClassnamesFormat
  { focusedFormatClassname   :: String -> String  -- ^ Focused formatting.
  , unfocusedFormatClassname :: String -> String  -- ^ Unfocused formatting.
  , urgentFormatClassname    :: String -> String  -- ^ Formatting when urgent.
  }

-- | How to format these classnames by default when using 'logClassnames'' and
-- 'logClassnamesOnScreen''.
instance Default ClassnamesFormat where
  def = ClassnamesFormat
    { focusedFormatClassname   = xmobarFocusedFormat
    , unfocusedFormatClassname = xmobarWsFormat
    , urgentFormatClassname    = xmobarUrgentFormat
    }

-- | Internal function to get the specified window information for all windows on
-- the visible workspace of the given screen and format them according to the
-- given functions.
logWindowInfoOnScreen
  :: (Window -> X String)
  -> ScreenId
  -> (String -> String)
  -> (String -> String)
  -> (String -> String)
  -> Logger
logWindowInfoOnScreen getWindowInfo sid formatFoc formatUnfoc formatUrg =
  (`withScreen` sid) $ \screen -> do
    let focWin = fmap W.focus . W.stack . W.workspace $ screen
    urgWins <- readUrgents
    logWindowInfoOnScreenWorker getWindowInfo screen $ \win name ->
      if | Just win == focWin -> formatFoc   name
         | win `elem` urgWins -> formatUrg   name
         | otherwise          -> formatUnfoc name

-- | Internal helper function for 'logWindowInfoOnScreen'.
logWindowInfoOnScreenWorker
  :: (Window -> X String)
  -> WindowScreen
  -> (Window -> String -> String)
  -> Logger
logWindowInfoOnScreenWorker getWindowInfo screen logger = do
  let wins = maybe [] W.integrate . W.stack . W.workspace $ screen
  winNames <- traverse getWindowInfo wins
  pure . Just . unwords $ zipWith logger wins winNames

-- | Internal. Like 'logWindowInfoOnScreen', but directly use the "focused" screen
-- (the one with the currently focused workspace).
logWindowInfoFocusedScreen
  :: (Window -> X String)
  -> (String -> String)
  -> (String -> String)
  -> (String -> String)
  -> Logger
logWindowInfoFocusedScreen getWindowInfo formatFoc formatUnfoc formatUrg = do
  sid <- gets $ W.screen . W.current . windowset
  logWindowInfoOnScreen getWindowInfo sid formatFoc formatUnfoc formatUrg

-- | Internal function to get the specified information for the currently focused window
logWindowInfoFocusedWindow :: (Window -> X String) -> Logger
logWindowInfoFocusedWindow getWindowInfo = withWindowSet $ traverse getWindowInfo . W.peek

-- | Internal formatting helpers
xmobarWsFormat, xmobarFocusedFormat, xmobarUrgentFormat :: String -> String
xmobarWsFormat      = xmobarRaw . shorten 30 . xmobarStrip
xmobarFocusedFormat = wrap "[" "]" . xmobarWsFormat
xmobarUrgentFormat  = wrap "!" "!" . xmobarWsFormat

-- | Get the name of the current layout.
logLayout :: Logger
logLayout = withWindowSet $ return . Just . ld
  where ld = description . W.layout . W.workspace . W.current

-- | Get the name of the current workspace.
logCurrent :: Logger
logCurrent = withWindowSet $ return . Just . W.currentTag

-- | Log the given string, as is.
logConst :: String -> Logger
logConst = return . Just

-- | If the first logger returns @Nothing@, the default logger is used.
-- For example, to display a quote when no windows are on the screen,
-- you can do:
--
-- > logDefault logTitle (logConst "Hey, you, you're finally awake.")
logDefault :: Logger -> Logger -> Logger
logDefault l d = l >>= maybe d logConst

-- | An infix operator for 'logDefault', which can be more convenient to
-- combine multiple loggers.
--
-- > logTitle .| logWhenActive 0 (logConst "*") .| logConst "There's nothing here"
(.|) :: Logger -> Logger -> Logger
(.|) = logDefault

-- $xmonad-screen
-- It is also possible to bind loggers like 'logTitle' to a specific screen. For
-- example, using @logTitleOnScreen 1@ will log the title of the focused window
-- on screen 1, even if screen 1 is not currently active.

-- | Only display the 'Logger' if the screen with the given 'ScreenId' is
-- active.
-- For example, this can be used to create a marker that is only displayed
-- when the primary screen is active.
--
-- > logWhenActive 0 (logConst "*")
logWhenActive :: ScreenId -> Logger -> Logger
logWhenActive n l = do
  c <- withWindowSet $ return . W.screen . W.current
  if n == c then l else return Nothing

-- | Get the title (name) of the focused window, on the given screen.
logTitleOnScreen :: ScreenId -> Logger
logTitleOnScreen = logWindowInfoFocusedWindowOnScreen fetchWindowTitle

-- | Get the classname of the focused window, on the given screen.
logClassnameOnScreen :: ScreenId -> Logger
logClassnameOnScreen = logWindowInfoFocusedWindowOnScreen fetchWindowClassname

-- | Internal function to get the specified information for the focused window,
-- on the given screen.
logWindowInfoFocusedWindowOnScreen :: (Window -> X String) -> ScreenId -> Logger
logWindowInfoFocusedWindowOnScreen getWindowInfo =
  withScreen
    $ traverse getWindowInfo
    . (W.focus <$>)
    . W.stack
    . W.workspace

-- | Get the name of the visible workspace on the given screen.
logCurrentOnScreen :: ScreenId -> Logger
logCurrentOnScreen = withScreen $ logConst . W.tag . W.workspace

-- | Get the name of the current layout on the given screen.
logLayoutOnScreen :: ScreenId -> Logger
logLayoutOnScreen =
  withScreen $ logConst . description . W.layout . W.workspace

-- | A helper function to create screen-specific loggers.
withScreen :: (WindowScreen -> Logger) -> ScreenId -> Logger
withScreen f n = do
  ss <- withWindowSet $ return . W.screens
  case find ((== n) . W.screen) ss of
    Just s  -> f s
    Nothing -> pure Nothing

-- $format
-- Combine logger formatting functions to make your
-- 'XMonad.Hooks.StatusBar.PP.ppExtras' more colorful and readable.
-- (For convenience, you can use '<$>' instead of \'.\' or \'$\' in hard to read
-- formatting lines.
-- For example:
--
-- > myPP = def {
-- >     -- skipped
-- >     , ppExtras = [lLoad, lTitle, logSp 3, wrapL "[" "]" $ date "%a %d %b"]
-- >     , ppOrder = \(ws:l:_:xs) -> [l,ws] ++ xs
-- >     }
-- >   where
-- >     -- lTitle = fixedWidthL AlignCenter "." 99 . dzenColorL "cornsilk3" "" . padL . shortenL 80 $ logTitle
-- >     -- or something like:
-- >     lTitle = fixedWidthL AlignCenter "." 99 <$> dzenColorL "cornsilk3" "" <$> padL . shortenL 80 $ logTitle
-- >
-- >     lLoad = dzenColorL "#6A5ACD" "" . wrapL loadIcon "   " . padL $ loadAvg
-- >     loadIcon = " ^i(/home/me/.dzen/icons/load.xbm)"
--
-- For more information on how to add the pretty-printer to your status bar, please
-- check "XMonad.Hooks.StatusBar".
--
-- Note: When applying 'shortenL' or 'fixedWidthL' to logger strings
-- containing colors or other formatting commands, apply the formatting
-- /after/ the length adjustment, or include \"invisible\" characters
-- in the length specification, e.g. in the above \'^fg(cornsilk3)\' and
-- \'^fg()' yields 19 invisible and 80 visible characters.

-- | Use a string formatting function to edit a 'Logger' string.
-- For example, to create a tag function to prefix or label loggers,
-- as in \'tag: output\', use:
--
-- > tagL l = onLogger $ wrap (l ++ ": ") ""
-- >
-- >    tagL "bat" battery
-- >    tagL "load" loadAvg
--
-- If you already have a (String -> String) function you want to
-- apply to a logger:
--
-- > revL = onLogger trim
--
-- See formatting utility source code for more 'onLogger' usage examples.
onLogger :: (String -> String) -> Logger -> Logger
onLogger = fmap . fmap

-- | Wrap a logger's output in delimiters, unless it is @X (Nothing)@
-- or @X (Just \"\")@. Some examples:
--
-- >    wrapL " | " " | " (date "%a %d %b") -- ' | Tue 19 Feb | '
-- >
-- >    wrapL "bat: " "" battery            -- ' bat: battery_logger_output'
wrapL :: String -> String -> Logger -> Logger
wrapL l r = onLogger $ wrap l r

-- | Make a logger's output constant width by padding with the given string,
-- /even if the logger is/ @X (Nothing)@ /or/ @X (Just \"\")@. Useful to
-- reduce visual noise as a title logger shrinks and grows, to use a fixed
-- width for a logger that sometimes becomes Nothing, or even to create
-- fancy spacers or character based art effects.
--
-- It fills missing logger output with a repeated character like \".\",
-- \":\" or pattern, like \" -.-\". The cycling padding string is reversed on
-- the left of the logger output. This is mainly useful with AlignCenter.
fixedWidthL :: Align  -- ^ AlignCenter, AlignRight, or AlignLeft
            -> String -- ^ String to cycle to pad missing logger output
            -> Int    -- ^ Fixed length to output (including invisible formatting characters)
            -> Logger -> Logger
fixedWidthL a str n logger = do
    mbl <- logger
    let l = fromMaybe "" mbl
    case a of
       AlignCenter -> toL (take n $ padhalf l ++ l ++ cs)
       AlignRight -> toL (reverse (take n $ reverse l ++ cs))
       _ -> toL (take n $ l ++ cs)
  where
    toL = return . Just
    cs  = cycle str
    padhalf x = reverse $ take ((n - length x) `div` 2) cs

-- | Create a \"spacer\" logger, e.g. @logSp 3 -- loggerizes \'   \'@.
-- For more complex \"spacers\", use 'fixedWidthL' with @return Nothing@.
logSp :: Int -> Logger
logSp n = return . Just . take n $ cycle " "

-- | Pad a logger's output with a leading and trailing space, unless it
-- is @X (Nothing)@ or @X (Just \"\")@.
padL :: Logger -> Logger
padL = onLogger pad

-- | Limit a logger's length, adding \"...\" if truncated.
shortenL :: Int -> Logger -> Logger
shortenL = onLogger . shorten

-- | Color a logger's output with dzen foreground and background colors.
--
-- >  dzenColorL "green" "#2A4C3F" battery
dzenColorL :: String -> String -> Logger -> Logger
dzenColorL fg bg = onLogger $ dzenColor fg bg

-- | Color a logger's output with xmobar foreground and background colors.
--
-- >  xmobarColorL "#6A5ACD" "gray6" loadAverage
xmobarColorL :: String -> String -> Logger -> Logger
xmobarColorL fg bg = onLogger $ xmobarColor fg bg

-- todo
-- * dynamicLogXinerama logger? Or sorted onscreen Id's with "current" indicator?
-- is logCurrent really useful at all?
--
-- * ppVisible, etc. Resolve code dup. somehow. Refactor DynamicLog so can
-- be used for regular PP stuff /and/ loggers?
--
-- * fns for "ppExtras as a whole", combine loggers more nicely.
--
-- * parsers  to use with fixedWidthL to be smarter about invisible characters?
