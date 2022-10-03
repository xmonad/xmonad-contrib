{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Util.Run
-- Description :  Several commands, as well as an EDSL, to run external processes.
-- Copyright   :  (C) 2007  Spencer Janssen, Andrea Rossato, glasser@mit.edu
--                    2022  Tony Zorman
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Tony Zorman <soliditsallgood@mailbox.org>
-- Stability   :  unstable
-- Portability :  unportable
--
-- This module provides several commands to run an external process.
-- Additionally, it provides an abstraction—particularly geared towards
-- programs like terminals or Emacs—to specify these processes from
-- XMonad in a compositional way.
--
-- Originally, this module was composed of functions formerly defined in
-- "XMonad.Util.Dmenu" (by Spencer Janssen), "XMonad.Util.Dzen" (by
-- glasser\@mit.edu) and @XMonad.Util.RunInXTerm@ (by Andrea Rossato).
-----------------------------------------------------------------------------

module XMonad.Util.Run (
  -- * Usage
  -- $usage
  runProcessWithInput,
  runProcessWithInputAndWait,
  safeSpawn,
  safeSpawnProg,
  unsafeSpawn,
  runInTerm,
  safeRunInTerm,
  seconds,
  spawnPipe,
  spawnPipeWithLocaleEncoding,
  spawnPipeWithUtf8Encoding,
  spawnPipeWithNoEncoding,

  -- * Compositionally Spawning Processes #EDSL#
  -- $EDSL

  -- ** Configuration and Running
  ProcessConfig (..),
  Input,
  spawnExternalProcess,
  proc,
  getInput,

  -- ** Programs
  inEditor,
  inTerm,
  termInDir,
  inProgram,

  -- ** General Combinators
  (>->),
  (>-$),
  inWorkingDir,
  eval,
  execute,
  executeNoQuote,
  setXClass,
  asString,

  -- ** Emacs Integration
  EmacsLib (..),
  setFrameName,
  withEmacsLibs,
  inEmacs,
  elispFun,
  asBatch,
  require,
  progn,
  quote,
  findFile,
  list,
  saveExcursion,

  -- * Re-exports
  hPutStr,
  hPutStrLn,
) where

import XMonad
import XMonad.Prelude
import qualified XMonad.Util.ExtensibleConf as XC

import Codec.Binary.UTF8.String (encodeString)
import Control.Concurrent (threadDelay)
import System.Directory (getDirectoryContents)
import System.IO
import System.Posix.IO
import System.Posix.Process (createSession, executeFile, forkProcess)
import System.Process (runInteractiveProcess)

{- $usage

You can use this module by importing it in your @xmonad.hs@

> import XMonad.Util.Run

It then all depends on what you want to do:

  - If you want to compositionally spawn programs, see [the relevant
    extended documentation](#g:EDSL).

  - For an example usage of 'runInTerm' see "XMonad.Prompt.Ssh".

  - For an example usage of 'runProcessWithInput' see
    "XMonad.Prompt.DirectoryPrompt", "XMonad.Util.Dmenu",
    "XMonad.Prompt.ShellPrompt", "XMonad.Actions.WmiiActions", or
    "XMonad.Prompt.WorkspaceDir".

  - For an example usage of 'runProcessWithInputAndWait' see
    "XMonad.Util.Dzen".
-}

-- | Returns the output.
runProcessWithInput :: MonadIO m => FilePath -> [String] -> String -> m String
runProcessWithInput cmd args input = io $ do
    (pin, pout, perr, _) <- runInteractiveProcess (encodeString cmd)
                                            (map encodeString args) Nothing Nothing
    hPutStr pin input
    hClose pin
    output <- hGetContents pout
    when (output == output) $ return ()
    hClose pout
    hClose perr
    -- no need to waitForProcess, we ignore SIGCHLD
    return output

-- | Wait is in &#956; (microseconds)
runProcessWithInputAndWait :: MonadIO m => FilePath -> [String] -> String -> Int -> m ()
runProcessWithInputAndWait cmd args input timeout = io $ do
    _ <- xfork $ do
        (pin, pout, perr, _) <- runInteractiveProcess (encodeString cmd)
                                            (map encodeString args) Nothing Nothing
        hPutStr pin input
        hFlush pin
        threadDelay timeout
        hClose pin
        hClose pout
        hClose perr
        -- no need to waitForProcess, we ignore SIGCHLD
        return ()
    return ()

-- | Multiplies by ONE MILLION, for functions that take microseconds.
--
-- Use like:
--
-- > (5.5 `seconds`)
--
-- In GHC 7 and later, you must either enable the PostfixOperators extension
-- (by adding
--
-- > {-# LANGUAGE PostfixOperators #-}
--
-- to the top of your file) or use seconds in prefix form:
--
-- > seconds 5.5
seconds :: Rational -> Int
seconds = fromEnum . (* 1000000)

{- | 'safeSpawn' bypasses 'spawn', because spawn passes
strings to \/bin\/sh to be interpreted as shell commands. This is
often what one wants, but in many cases the passed string will contain
shell metacharacters which one does not want interpreted as such (URLs
particularly often have shell metacharacters like \'&\' in them). In
this case, it is more useful to specify a file or program to be run
and a string to give it as an argument so as to bypass the shell and
be certain the program will receive the string as you typed it.

Examples:

> , ((modm, xK_Print), unsafeSpawn "import -window root $HOME/xwd-$(date +%s)$$.png")
> , ((modm, xK_d    ), safeSpawn "firefox" [])

Note that the unsafeSpawn example must be unsafe and not safe because
it makes use of shell interpretation by relying on @$HOME@ and
interpolation, whereas the safeSpawn example can be safe because
Firefox doesn't need any arguments if it is just being started. -}
safeSpawn :: MonadIO m => FilePath -> [String] -> m ()
safeSpawn prog args = io $ void_ $ forkProcess $ do
  uninstallSignalHandlers
  _ <- createSession
  executeFile (encodeString prog) True (map encodeString args) Nothing
    where void_ = (>> return ()) -- TODO: replace with Control.Monad.void / void not in ghc6 apparently

-- | Simplified 'safeSpawn'; only takes a program (and no arguments):
--
-- > , ((modm, xK_d    ), safeSpawnProg "firefox")
safeSpawnProg :: MonadIO m => FilePath -> m ()
safeSpawnProg = flip safeSpawn []

-- | An alias for 'spawn'; the name emphasizes that one is calling out to a
--   Turing-complete interpreter which may do things one dislikes; for details, see 'safeSpawn'.
unsafeSpawn :: MonadIO m => String -> m ()
unsafeSpawn = spawn

-- | Open a terminal emulator. The terminal emulator is specified in the default configuration as xterm by default. It is then
-- asked to pass the shell a command with certain options. This is unsafe in the sense of 'unsafeSpawn'
unsafeRunInTerm, runInTerm :: String -> String -> X ()
unsafeRunInTerm options command = asks (terminal . config) >>= \t -> unsafeSpawn $ t ++ " " ++ options ++ " -e " ++ command
runInTerm = unsafeRunInTerm

-- | Run a given program in the preferred terminal emulator; see 'runInTerm'. This makes use of 'safeSpawn'.
safeRunInTerm :: String -> String -> X ()
safeRunInTerm options command = asks (terminal . config) >>= \t -> safeSpawn t [options, " -e " ++ command]

-- | Launch an external application through the system shell and
-- return a 'Handle' to its standard input. Note that the 'Handle'
-- is a text 'Handle' using the current locale encoding.
spawnPipe :: MonadIO m => String -> m Handle
spawnPipe = spawnPipeWithLocaleEncoding

-- | Same as 'spawnPipe'.
spawnPipeWithLocaleEncoding :: MonadIO m => String -> m Handle
spawnPipeWithLocaleEncoding = spawnPipe' localeEncoding

-- | Same as 'spawnPipe', but forces the UTF-8 encoding regardless of locale.
spawnPipeWithUtf8Encoding :: MonadIO m => String -> m Handle
spawnPipeWithUtf8Encoding = spawnPipe' utf8

-- | Same as 'spawnPipe', but forces the 'char8' encoding, so unicode strings
-- need 'Codec.Binary.UTF8.String.encodeString'. Should never be needed, but
-- some X functions return already encoded Strings, so it may possibly be
-- useful for someone.
spawnPipeWithNoEncoding :: MonadIO m => String -> m Handle
spawnPipeWithNoEncoding = spawnPipe' char8

spawnPipe' :: MonadIO m => TextEncoding -> String -> m Handle
spawnPipe' encoding x = io $ do
    (rd, wr) <- createPipe
    setFdOption wr CloseOnExec True
    h <- fdToHandle wr
    hSetEncoding h encoding
    hSetBuffering h LineBuffering
    _ <- xfork $ do
          _ <- dupTo rd stdInput
          executeFile "/bin/sh" False ["-c", encodeString x] Nothing
    closeFd rd
    return h

{- $EDSL

To use the provided EDSL, you must first add the 'spawnExternalProcess'
combinator to your xmonad configuration, like so:

> main = xmonad $ … $ spawnExternalProcess def $ … $ def

See 'ProcessConfig' for a list of all default configuration options, in
case you'd like to change them—especially if you want to make use of the
Emacs integration.

After that, the real fun begins!  The format for spawning these
processes is always the same: a call to 'proc', its argument being a
bunch of function calls, separated by the pipe operator '(>->)'.  You
can just bind the resulting function to a key; no additional plumbing
required.  For example, using "XMonad.Util.EZConfig" syntax and with
@terminal = "alacritty"@ in you XMonad configuration, spawning a @ghci@
session with a special class name, "calculator", would look like

> ("M-y", proc $ inTerm >-> setXClass "calculator" >-> execute "ghci")

which would translate, more or less, to @\/usr\/bin\/sh -c "alacritty
--class calculator -e ghci"@.  The usefulness of this notation becomes
apparent with more complicated examples:

> proc $ inEmacs
>    >-> withEmacsLibs [OwnFile "mailboxes"]
>    >-> execute (elispFun "notmuch")
>    >-> setFrameName "mail"

This is equivalent to spawning

> emacs -l /home/slot/.config/emacs/lisp/mailboxes.el
>       -e '(notmuch)'
>       -F '(quote (name . "mail"))'

Notice how we did not have to specify the whole path to @mailboxes.el@,
since we had set the correct 'emacsLispDir' upon starting xmonad.  This
becomes especially relevant when running Emacs in batch mode, where one
has to include [M,Non-GNU]ELPA packages in the call, whose exact names
may change at any time.  Then the following

> do url <- getSelection  -- from XMonad.Util.XSelection
>    proc $ inEmacs
>       >-> withEmacsLibs [ElpaLib "dash", ElpaLib "s", OwnFile "arXiv-citation"]
>       >-> asBatch
>       >-> execute (elispFun $ "arXiv-citation" <> asString url)

becomes

> emacs -L /home/slot/.config/emacs/elpa/dash-20220417.2250
>       -L /home/slot/.config/emacs/elpa/s-20210616.619
>       -l /home/slot/.config/emacs/lisp/arXiv-citation.el
>       --batch
>       -e '(arXiv-citation "<url-in-the-primary-selection>")'

which would be quite bothersome to type indeed!

A blog post going into some more detail and also explaining how to
integrate this new language with the "XMonad.Util.NamedScratchpad"
module is available
<https://tony-zorman.com/posts/2022-05-25-calling-emacs-from-xmonad.html here>.
-}

-----------------------------------------------------------------------
-- Types and whatnot

-- | Additional information that might be useful when spawning external
-- programs.
data ProcessConfig = ProcessConfig
  { editor :: !String
    -- ^ Default editor.  Defaults to @"emacsclient -c -a ''"@.
  , emacsLispDir :: !FilePath
    -- ^ Directory for your custom Emacs lisp files.  Probably
    -- @user-emacs-directory@ or @user-emacs-directory/lisp@.  Defaults
    -- to @"~\/.config\/emacs\/lisp\/"@
  , emacsElpaDir :: !FilePath
    -- ^ Directory for all packages from [M,Non-GNU]ELPA; probably
    -- @user-emacs-directory/elpa@.  Defaults to
    -- @"~\/.config\/emacs\/elpa"@.
  , emacs :: !String
    -- ^ /Standalone/ Emacs executable; this should not be @emacsclient@
    -- since, for example, the client does not support @--batch@ mode.
    -- Defaults to @"emacs"@.
  }

-- | Given a 'ProcessConfig', remember it for spawning external
-- processes later on.
spawnExternalProcess :: ProcessConfig -> XConfig l -> XConfig l
spawnExternalProcess = XC.modifyDef . const

instance Default ProcessConfig where
  def :: ProcessConfig
  def = ProcessConfig
    { editor       = "emacsclient -c -a ''"
    , emacsLispDir = "~/.config/emacs/lisp/"
    , emacsElpaDir = "~/.config/emacs/elpa/"
    , emacs        = "emacs"
    }

-- | Convenient type alias.
type Input = ShowS

-----------------------------------------------------------------------
-- Combinators

-- | Combine inputs together.
(>->) :: X Input -> X Input -> X Input
(>->) = (<>)
infixr 3 >->

-- | Combine an input with an ordinary string.
(>-$) :: X Input -> X String -> X Input
(>-$) xi xs = xi >-> fmap mkDList xs
infixr 3 >-$

-- | Spawn a completed input.
proc :: X Input -> X ()
proc xi = spawn =<< getInput xi

-- | Get the completed input string.
getInput :: X Input -> X String
getInput xi = xi <&> ($ "")

-- | Use the 'editor'.
inEditor :: X Input
inEditor = XC.withDef $ \ProcessConfig{editor} -> pure $ mkDList editor

-- | Use the 'XMonad.Core.XConfig.terminal'.
inTerm :: X Input
inTerm = asks $ mkDList . terminal . config

-- | Execute the argument.  Current /thing/ must support a @-e@ option.
-- For programs such as Emacs, 'eval' may be the safer option; while
-- @emacsclient@ supports @-e@, the @emacs@ executable itself does not.
--
-- Note that this function always wraps its argument in single quotes;
-- see 'executeNoQuote' for an alternative.
execute :: String -> X Input
execute this = pure ((" -e " <> tryQuote this) <>)

-- | Like 'execute', but doesn't wrap its argument in single quotes.
executeNoQuote :: String -> X Input
executeNoQuote this = pure ((" -e " <> this) <>)

-- | Eval(uate) the argument.  Current /thing/ must support a @--eval@
-- option.
eval :: String -> X Input
eval this = pure ((" --eval " <> tryQuote this) <>)

-- | Use 'emacs'.
inEmacs :: X Input
inEmacs = XC.withDef $ \ProcessConfig{emacs} -> pure $ mkDList emacs

-- | Use the given program.
inProgram :: String -> X Input
inProgram = pure . mkDList

-- | Spawn /thing/ in the current working directory.  /thing/ must
-- support a @--working-directory@ option.
inWorkingDir :: X Input
inWorkingDir = pure (" --working-directory " <>)

-- | Set a frame name for the @emacsclient@.
--
-- Note that this uses the @-F@ option to set the
-- <https://www.gnu.org/software/emacs/manual/html_node/emacs/Frame-Parameters.html frame parameters>
-- alist, which the @emacs@ executable does not support.
setFrameName :: String -> X Input
setFrameName n = pure ((" -F '(quote (name . \"" <> n <> "\"))' ") <>)

-- | Set the appropriate X class for a window.  This will more often
-- than not actually be the
-- <https://tronche.com/gui/x/icccm/sec-4.html#WM_CLASS instance name>.
setXClass :: String -> X Input
setXClass = pure . mkDList . (" --class " <>)

-- | Spawn the 'XMonad.Core.XConfig.terminal' in some directory; it must
-- support the @--working-directory@ option.
termInDir :: X Input
termInDir = inTerm >-> inWorkingDir

-----------------------------------------------------------------------
-- Emacs

-- | Transform the given input into an elisp function; i.e., surround it
-- with parentheses.
--
-- >>> elispFun "arxiv-citation URL"
-- " '( arxiv-citation URL )' "
elispFun :: String -> String
elispFun f = " '( " <> f <> " )' "

-- | Treat an argument as a string; i.e., wrap it with quotes.
--
-- >>> asString "string"
-- " \"string\" "
asString :: String -> String
asString s = " \"" <> s <> "\" "

-- | Wrap the given commands in a @progn@ and also escape it by wrapping
-- it inside single quotes.  The given commands need not be wrapped in
-- parentheses, this will be done by the function.  For example:
--
-- >>> progn [require "this-lib", "function-from-this-lib arg", "(other-function arg2)"]
-- " '( progn (require (quote this-lib)) (function-from-this-lib arg) (other-function arg2) )' "
progn :: [String] -> String
progn cmds = elispFun $ "progn " <> unwords (map inParens cmds)

-- | Require a package.
--
-- >>> require "arxiv-citation"
-- "(require (quote arxiv-citation))"
require :: String -> String
require = inParens . ("require " <>) . quote

-- | Quote a symbol.
--
-- >>> quote "new-process"
-- "(quote new-process)"
quote :: String -> String
quote = inParens . ("quote " <>)

-- | Call @find-file@.
--
-- >>> findFile "/path/to/file"
-- "(find-file \"/path/to/file\" )"
findFile :: String -> String
findFile = inParens . ("find-file" <>) . asString

-- | Make a list of the given inputs.
--
-- >>> list ["foo", "bar", "baz", "qux"]
-- "(list foo bar baz qux)"
list :: [String] -> String
list = inParens . ("list " <>) . unwords

-- | Like 'progn', but with @save-excursion@.
--
-- >>> saveExcursion [require "this-lib", "function-from-this-lib arg", "(other-function arg2)"]
-- "(save-excursion (require (quote this-lib)) (function-from-this-lib arg) (other-function arg2))"
saveExcursion :: [String] -> String
saveExcursion = inParens . ("save-excursion " <>) . unwords . map inParens

-----------------------------------------------------------------------
-- Batch mode

-- | Tell Emacs to enable batch-mode.
asBatch :: X Input
asBatch = pure (" --batch " <>)

-- | An Emacs library.
data EmacsLib
  = OwnFile !String
    -- ^ A /file/ from 'emacsLispDir'.
  | ElpaLib !String
    -- ^ A /directory/ in 'emacsElpaDir'.
  | Special !String
    -- ^ Special /files/; these will not be looked up somewhere, but
    -- forwarded verbatim (as a path).

-- | Load some Emacs libraries.  This is useful when executing scripts
-- in batch mode.
withEmacsLibs :: [EmacsLib] -> X Input
withEmacsLibs libs = XC.withDef $ \ProcessConfig{emacsLispDir, emacsElpaDir} -> do
  lispDir <- mkAbsolutePath emacsLispDir
  elpaDir <- mkAbsolutePath emacsElpaDir
  lisp    <- liftIO $ getDirectoryContents lispDir
  elpa    <- liftIO $ getDirectoryContents elpaDir

  let getLib :: EmacsLib -> Maybe String = \case
        OwnFile f -> (("-l " <> lispDir) <>) <$> find (f          `isPrefixOf`) lisp
        ElpaLib d -> (("-L " <> elpaDir) <>) <$> find ((d <> "-") `isPrefixOf`) elpa
        Special f -> Just $ " -l " <> f
  pure . mkDList . unwords . mapMaybe getLib $ libs

-----------------------------------------------------------------------
-- Util

mkDList :: String -> ShowS
mkDList = (<>) . (<> " ")

inParens :: String -> String
inParens s = case s of
  '(' : _ -> s
  _       -> "(" <> s <> ")"

tryQuote :: String -> String
tryQuote s = case dropWhile (== ' ') s of
  '\'' : _ -> s
  _        -> "'" <> s <> "'"
