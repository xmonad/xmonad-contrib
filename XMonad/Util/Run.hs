-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Util.Run
-- Copyright   :  (C) 2007 Spencer Janssen, Andrea Rossato, glasser@mit.edu
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Christian Thiemann <mail@christian-thiemann.de>
-- Stability   :  unstable
-- Portability :  unportable
--
-- This modules provides several commands to run an external process.
-- It is composed of functions formerly defined in "XMonad.Util.Dmenu" (by
-- Spencer Janssen), "XMonad.Util.Dzen" (by glasser\@mit.edu) and
-- XMonad.Util.RunInXTerm (by Andrea Rossato).
--
-----------------------------------------------------------------------------

module XMonad.Util.Run (
                          -- * Usage
                          -- $usage
                          runProcessWithInput,
                          runProcessWithInputAndWait,
                          safeSpawn,
                          unsafeSpawn,
                          runInTerm,
                          safeRunInTerm,
                          seconds,
                          spawnPipe,

                          hPutStr, hPutStrLn  -- re-export for convenience
                         ) where

import System.Posix.IO
import System.Posix.Process (executeFile, forkProcess, createSession)
import Control.Concurrent (threadDelay)
import Control.Exception (try)
import System.IO
import System.Process (runInteractiveProcess, waitForProcess)
import XMonad
import Control.Monad

-- $usage
-- For an example usage of 'runInTerm' see "XMonad.Prompt.Ssh"
--
-- For an example usage of 'runProcessWithInput' see
-- "XMonad.Prompt.DirectoryPrompt", "XMonad.Util.Dmenu",
-- "XMonad.Prompt.ShellPrompt", "XMonad.Actions.WmiiActions",
-- "XMonad.Prompt.WorkspaceDir"
--
-- For an example usage of 'runProcessWithInputAndWait' see
-- "XMonad.Util.Dzen"

-- | Return output if the command succeeded, otherwise return @()@.
-- This corresponds to dmenu's notion of exit code 1 for a cancelled invocation.
runProcessWithInput :: FilePath -> [String] -> String -> IO String
runProcessWithInput cmd args input = do
    (pin, pout, perr, _) <- runInteractiveProcess cmd args Nothing Nothing
    hPutStr pin input
    hClose pin
    output <- hGetContents pout
    when (output == output) $ return ()
    hClose pout
    hClose perr
    -- no need to waitForProcess, we ignore SIGCHLD
    return output

-- | Wait is in µs (microseconds)
runProcessWithInputAndWait :: FilePath -> [String] -> String -> Int -> IO ()
runProcessWithInputAndWait cmd args input timeout = do
    forkProcess $ do
        (pin, pout, perr, ph) <- runInteractiveProcess cmd args Nothing Nothing
        hPutStr pin input
        hFlush pin
        threadDelay timeout
        hClose pin
        hClose pout
        hClose perr
        waitForProcess ph
        return ()
    return ()

-- | Multiplies by ONE MILLION, for functions that take microseconds.
--
-- Use like:
--
-- > (5.5 `seconds`)
seconds :: Rational -> Int
seconds = fromEnum . (* 1000000)

{- | 'safeSpawn' bypasses "XMonad.Core"'s 'spawn' command, because spawn passes
strings to \/bin\/sh to be interpreted as shell commands. This is
often what one wants, but in many cases the passed string will contain
shell metacharacters which one does not want interpreted as such (URLs
particularly often have shell metacharacters like \'&\' in them). In
this case, it is more useful to specify a file or program to be run
and a string to give it as an argument so as to bypass the shell and
be certain the program will receive the string as you typed it.
unsafeSpawn is internally an alias for XMonad's 'spawn', to remind one that use
of it can be, well, unsafe.
Examples:

> , ((modMask, xK_Print), unsafeSpawn "import -window root $HOME/xwd-$(date +%s)$$.png")
> , ((modMask, xK_d    ), safeSpawn "firefox" "")

Note that the unsafeSpawn example must be unsafe and not safe because
it makes use of shell interpretation by relying on @$HOME@ and
interpolation, whereas the safeSpawn example can be safe because
Firefox doesn't need any arguments if it is just being started. -}
safeSpawn :: MonadIO m => FilePath -> String -> m ()
safeSpawn prog arg = liftIO (try (forkProcess $ executeFile prog True [arg] Nothing) >> return ())

unsafeSpawn :: MonadIO m => String -> m ()
unsafeSpawn = spawn

-- | Open a terminal emulator. The terminal emulator is specified in @defaultConfig@ as xterm by default. It is then
-- asked to pass the shell a command with certain options. This is unsafe in the sense of 'unsafeSpawn'
unsafeRunInTerm, runInTerm :: String -> String -> X ()
unsafeRunInTerm options command = asks (terminal . config) >>= \t -> unsafeSpawn $ t ++ " " ++ options ++ " -e " ++ command
runInTerm = unsafeRunInTerm

-- | Run a given program in the preferred terminal emulator; see 'runInTerm'. This makes use of 'safeSpawn'.
safeRunInTerm :: String -> String -> X ()
safeRunInTerm options command = asks (terminal . config) >>= \t -> safeSpawn t (options ++ " -e " ++ command)

-- | Launch an external application through the system shell and return a @Handle@ to its standard input.
spawnPipe :: String -> IO Handle
spawnPipe x = do
    (rd, wr) <- createPipe
    setFdOption wr CloseOnExec True
    h <- fdToHandle wr
    hSetBuffering h LineBuffering
    forkProcess $ do
        createSession
        uninstallSignalHandlers
        dupTo rd stdInput
        executeFile "/bin/sh" False ["-c", x] Nothing
    return h
