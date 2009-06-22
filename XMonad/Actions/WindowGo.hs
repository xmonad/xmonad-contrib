{- |
Module      :  XMonad.Actions.WindowGo
License     :  Public domain

Maintainer  :  <gwern0@gmail.com>
Stability   :  unstable
Portability :  unportable

Defines a few convenient operations for raising (traveling to) windows based on XMonad's Query
monad, such as 'runOrRaise'. runOrRaise will run a shell command unless it can
find a specified window; you would use this to automatically travel to your
Firefox or Emacs session, or start a new one (for example), instead of trying to
remember where you left it or whether you still have one running. -}

module XMonad.Actions.WindowGo (
                 -- * Usage
                 -- $usage
                 raise,
                 raiseNext,
                 runOrRaise,
                 runOrRaiseNext,
                 raiseMaybe,
                 raiseNextMaybe,

                 raiseBrowser,
                 raiseEditor,
                 runOrRaiseAndDo,
                 runOrRaiseMaster,
                 raiseAndDo,
                 raiseMaster,
                 module XMonad.ManageHook
                ) where

import Control.Monad (filterM)
import Data.Char (toLower)

import XMonad (Query(), X(), withWindowSet, spawn, runQuery, liftIO)
import Graphics.X11 (Window)
import XMonad.ManageHook
import XMonad.Operations (windows)
import XMonad.Prompt.Shell (getBrowser, getEditor)
import qualified XMonad.StackSet as W (allWindows, peek, swapMaster, focusWindow)
{- $usage

Import the module into your @~\/.xmonad\/xmonad.hs@:

> import XMonad.Actions.WindowGo

and define appropriate key bindings:

> , ((modMask x .|. shiftMask, xK_g), raise (className =? "Firefox"))
> , ((modMask x .|. shiftMask, xK_b), runOrRaise "firefox" (className =? "Firefox"))

(Note that Firefox v3 and up have a class-name of \"Firefox\" and \"Navigator\";
lower versions use other classnames such as \"Firefox-bin\". Either choose the
appropriate one, or cover your bases by using instead something like
 @(className =? \"Firefox\" <||> className =? \"Firefox-bin\")@.)

For detailed instructions on editing your key bindings, see
"XMonad.Doc.Extending#Editing_key_bindings". -}

-- | 'action' is an executable to be run via 'spawn' (of "XMonad.Core") if the Window cannot be found.
--   Presumably this executable is the same one that you were looking for.
runOrRaise :: String -> Query Bool -> X ()
runOrRaise = raiseMaybe . spawn

-- | See 'raiseMaybe'. If the Window can't be found, quietly give up and do nothing.
raise :: Query Bool -> X ()
raise = raiseMaybe $ return ()

{- | 'raiseMaybe' queries all Windows based on a boolean provided by the
   user. Currently, there are 3 such useful booleans defined in
   "XMonad.ManageHook": 'title', 'resource', 'className'. Each one tests based pretty
   much as you would think. ManageHook also defines several operators, the most
   useful of which is (=?). So a useful test might be finding a @Window@ whose
   class is Firefox. Firefox 3 declares the class \"Firefox\", so you'd want to
   pass in a boolean like @(className =? \"Firefox\")@.

   If the boolean returns @True@ on one or more windows, then XMonad will quickly
   make visible the first result. If no @Window@ meets the criteria, then the
   first argument comes into play.

   The first argument is an arbitrary IO function which will be executed if the
   tests fail. This is what enables 'runOrRaise' to use 'raiseMaybe': it simply runs
   the desired program if it isn't found. But you don't have to do that. Maybe
   you want to do nothing if the search fails (the definition of 'raise'), or
   maybe you want to write to a log file, or call some prompt function, or
   something crazy like that. This hook gives you that flexibility. You can do
   some cute things with this hook. Suppose you want to do the same thing for
   Mutt which you just did for Firefox - but Mutt runs inside a terminal window?
   No problem: you search for a terminal window calling itself \"mutt\", and if
   there isn't you run a terminal with a command to run Mutt! Here's an example
   (borrowing 'runInTerm' from "XMonad.Utils.Run"):

  > , ((modm, xK_m), raiseMaybe (runInTerm "-title mutt" "mutt") (title =? "mutt"))
-}
raiseMaybe :: X () -> Query Bool -> X ()
raiseMaybe f thatUserQuery = withWindowSet $ \s -> do
    maybeResult <- filterM (runQuery thatUserQuery) (W.allWindows s)
    case maybeResult of
      []     -> f
      (x:_)  -> windows $ W.focusWindow x

-- | See 'runOrRaise' and 'raiseNextMaybe'. Version that allows cycling through matches.
runOrRaiseNext :: String -> Query Bool -> X ()
runOrRaiseNext = raiseNextMaybe . spawn

-- | See 'raise' and 'raiseNextMaybe'. Version that allows cycling through matches.
raiseNext :: Query Bool -> X ()
raiseNext = raiseNextMaybe $ return ()

{- | See 'raiseMaybe'.
     'raiseNextMaybe' is an alternative version that allows cycling
     through the matching windows. If the focused window matches the
     query the next matching window is raised. If no matches are found
     the function f is executed.
-}
raiseNextMaybe :: X () -> Query Bool -> X ()
raiseNextMaybe f thatUserQuery = withWindowSet $ \s -> do
  ws <- filterM (runQuery thatUserQuery) (W.allWindows s)
  case ws of
    []    -> f
    (x:_) -> let go (Just w) | (w `elem` ws) = next w $ cycle ws
                 go _ = windows $ W.focusWindow x
             in go $ W.peek s
  where
    next w (x:y:_) | x==w = windows $ W.focusWindow y
    next w (_:xs)         = next w xs
    next _ _ = error "raiseNextMaybe: empty list"

-- | Given a function which gets us a String, we try to raise a window with that classname,
--   or we then interpret that String as a executable name.
raiseVar :: IO String -> X ()
raiseVar getvar = liftIO getvar >>= \var -> runOrRaise var (fmap (map toLower) className =? var)

{- | 'raiseBrowser' and 'raiseEditor' grab $BROWSER and $EDITOR respectively and they either
     take you to the specified program's window, or they try to run it. This is most useful
     if your variables are simple and look like \"firefox\" or \"emacs\". -}
raiseBrowser, raiseEditor :: X ()
raiseBrowser = raiseVar getBrowser
raiseEditor  = raiseVar getEditor

{- | If the window is found the window is focused and the third argument is called
     otherwise, the first argument is called
     See 'raiseMaster' for an example. -}
raiseAndDo :: X () -> Query Bool -> (Window -> X ())-> X ()
raiseAndDo raisef thatUserQuery afterRaise = withWindowSet $ \s -> do
    maybeResult <- filterM (runQuery thatUserQuery) (W.allWindows s)
    case maybeResult of
      []     -> raisef
      (x:_)  -> do windows $ W.focusWindow x
                   afterRaise x

{- | If a window matching the second arugment is found, the window is focused and the third argument is called;
     otherwise, the first argument is called. -}
runOrRaiseAndDo :: String -> Query Bool -> (Window -> X ()) -> X ()
runOrRaiseAndDo = raiseAndDo . spawn

{- | if the window is found the window is focused and set to master
     otherwise, the first argument is called.

     > raiseMaster (runInTerm \"-title ghci\"  \"zsh -c \'ghci\'\") (title =? \"ghci\") -}
raiseMaster :: X () -> Query Bool -> X ()
raiseMaster raisef thatUserQuery = raiseAndDo raisef thatUserQuery (\_ -> windows W.swapMaster)

{- |  If the window is found the window is focused and set to master
      otherwise, action is run.

      > runOrRaiseMaster \"firefox\" (className =? \"Firefox\")) 
  -}
runOrRaiseMaster :: String -> Query Bool -> X ()
runOrRaiseMaster run query = runOrRaiseAndDo run query (\_ -> windows W.swapMaster)
