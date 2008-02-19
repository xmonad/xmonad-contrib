-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Util.Loggers
-- Copyright   :  (c) Brent Yorgey
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  <byorgey@gmail.com>
-- Stability   :  unstable
-- Portability :  unportable
--
-- A collection of simple logger functions which can be used in the
-- 'XMonad.Hooks.DynamicLog.ppExtras' field of a pretty-printing status
-- logger format. See "XMonad.Hooks.DynamicLog" for more information.
-----------------------------------------------------------------------------

module XMonad.Util.Loggers (
                            -- * Usage
                            -- $usage

                             Logger

                           , date
                           , loadAvg
                           , battery
                           , logCmd

                           ) where

import XMonad.Core

import System.Time
import System.IO
import System.Process
import System.Locale

-- $usage
-- You can use this module by importing it into your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Util.Loggers
--
-- Then, add one or more loggers to the
-- 'XMonad.Hooks.DynamicLog.ppExtras' field of your
-- 'XMonad.Hooks.DynamicLoc.PP' format.  For example:
--
-- >     -- display load averages and a pithy quote along with xmonad status.
-- >   , logHook = dynamicLogWithPP $ defaultPP { ppExtras = [ loadAvg, logCmd "fortune -n 40 -s" ] }
--
-- Of course, there is nothing really special about these so-called
-- \'loggers\': they are just @X (Maybe String)@ actions.  So you can
-- use them anywhere you would use an @X (Maybe String)@, not just
-- with DynamicLog.
--
-- Additional loggers welcome!
--

-- | 'Logger' is just a convenient synonym for @X (Maybe String)@.
type Logger = X (Maybe String)

-- | Get the current date and time, and format them via the
--   given format string.  The format used is the same as that used
--   by the C library function strftime; for example,
--   @date \"%a %b %d\"@ might display something like @Tue Feb 19@.
--   For more information see something like
--   <http://www.cplusplus.com/reference/clibrary/ctime/strftime.html>.
date :: String -> Logger
date fmt = io $ do cal <- (getClockTime >>= toCalendarTime)
                   return . Just $ formatCalendarTime defaultTimeLocale fmt cal

-- | Get the load average.  This assumes that you have a
--   utility called @\/usr\/bin\/uptime@ and that you have @sed@
--   installed; these are fairly common on GNU\/Linux systems but it
--   would be nice to make this more general.
loadAvg :: Logger
loadAvg = logCmd "/usr/bin/uptime | sed 's/.*: //; s/,//g'"

-- | Get the battery status (percent charge and charging\/discharging
--   status). This is an ugly hack and may not work for some people.
--   At some point it would be nice to make this more general\/have
--   fewer dependencies.
battery :: Logger
battery = logCmd "/usr/bin/acpi | sed -r 's/.*?: (.*%).*/\\1/; s/discharging, ([0-9]+%)/\\1-/; s/charging, ([0-9]+%)/\\1+/; s/charged, //'"

-- | Create a 'Logger' from an arbitrary shell command.
logCmd :: String -> Logger
logCmd c = io $ do (_, out, _, proc) <- runInteractiveCommand c
                   output <- hGetLine out
                   waitForProcess proc
                   return $ Just output
