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
                          spawnPipe
                         ) where

import Control.Monad.Reader
import System.Posix.IO
import System.Posix.Process (createSession, forkProcess, executeFile,
                             getProcessStatus)
import Control.Concurrent (threadDelay)
import Control.Exception (try)
import System.Exit (ExitCode(ExitSuccess), exitWith)
import System.IO
import System.Process (runInteractiveProcess, waitForProcess)
import XMonad

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

-- | Returns Just output if the command succeeded, and Nothing if it didn't.
-- This corresponds to dmenu's notion of exit code 1 for a cancelled invocation.
runProcessWithInput :: FilePath -> [String] -> String -> IO String
runProcessWithInput cmd args input = do
    (pin, pout, perr, ph) <- runInteractiveProcess cmd args Nothing Nothing
    hPutStr pin input
    hClose pin
    output <- hGetContents pout
    when (output==output) $ return ()
    hClose pout
    hClose perr
    waitForProcess ph
    return output

-- | Wait is in us
runProcessWithInputAndWait :: FilePath -> [String] -> String -> Int -> IO ()
runProcessWithInputAndWait cmd args input timeout = do
    pid <- forkProcess $ do
       forkProcess $ do -- double fork it over to init
         createSession
         (pin, pout, perr, ph) <- runInteractiveProcess cmd args Nothing Nothing
         hPutStr pin input
         hFlush pin
         threadDelay timeout
         hClose pin
         hClose pout
         hClose perr
         waitForProcess ph
         return ()
       exitWith ExitSuccess
       return ()
    getProcessStatus True False pid
    return ()

-- | Multiplies by ONE MILLION, for use with
-- 'runProcessWithInputAndWait'.
--
-- Use like:
--
-- > (5.5 `seconds`)
seconds :: Rational -> Int
seconds = fromEnum . (* 1000000)

-- | safeSpawn bypasses XMonad's 'spawn' command, because 'spawn' passes
-- strings to \/bin\/sh to be interpreted as shell commands. This is
-- often what one wants, but in many cases the passed string will contain
-- shell metacharacters which one does not want interpreted as such (URLs
-- particularly often have shell metacharacters like \'&\' in them). In
-- this case, it is more useful to specify a file or program to be run
-- and a string to give it as an argument so as to bypass the shell and
-- be certain the program will receive the string as you typed it.
-- unsafeSpawn is an alias for XMonad's 'spawn', to remind one that use
-- of it can be, well, unsafe.
-- Examples:
-- 
-- >     , ((modMask, xK_Print), unsafeSpawn "import -window root png:$HOME/xwd-$(date +%s)$$.png")
-- >     , ((modMask, xK_d    ), safeSpawn "firefox" "")
-- 
-- Note that the unsafeSpawn example must be unsafe and not safe because
-- it makes use of shell interpretation by relying on @$HOME@ and
-- interpolation, whereas the safeSpawn example can be safe because
-- Firefox doesn't need any arguments if it is just being started.
safeSpawn :: MonadIO m => FilePath -> String -> m ()
safeSpawn prog arg = liftIO (try (forkProcess $ executeFile prog True [arg] Nothing) >> return ())

unsafeSpawn :: MonadIO m => String -> m ()
unsafeSpawn = spawn

-- | Run a given program in the preferred terminal emulator. This uses
-- 'safeSpawn'.
safeRunInTerm :: String -> X ()
safeRunInTerm command = asks (terminal . config) >>= \t -> safeSpawn t ("-e " ++ command)

unsafeRunInTerm, runInTerm :: String -> X ()
unsafeRunInTerm command = asks (terminal . config) >>= \t -> unsafeSpawn $ t ++ " -e " ++ command
runInTerm = unsafeRunInTerm

-- | Launch an external application and return a 'Handle' to its standard input.
spawnPipe :: String -> IO Handle
spawnPipe x = do
    (rd, wr) <- createPipe
    setFdOption wr CloseOnExec True
    h <- fdToHandle wr
    hSetBuffering h LineBuffering
    pid <- forkProcess $ do
        forkProcess $ do
            dupTo rd stdInput
            createSession
            executeFile "/bin/sh" False ["-c", x] Nothing
        exitWith ExitSuccess
    getProcessStatus True False pid
    return h
