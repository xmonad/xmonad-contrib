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
                 runOrRaise,
                 raiseMaybe,

                 raiseBrowser,
                 raiseEditor,
                 module XMonad.ManageHook
                ) where

import Control.Monad (filterM)
import Data.Char (toLower)

import XMonad (Query(), X(), withWindowSet, spawn, runQuery, liftIO, focus)
import XMonad.ManageHook
import XMonad.Prompt.Shell (getBrowser, getEditor)
import qualified XMonad.StackSet as W (allWindows)

{- $usage

Import the module into your @~\/.xmonad\/xmonad.hs@:

> import XMonad.Actions.WindowGo

and define appropriate key bindings:

> , ((modMask x .|. shiftMask, xK_g), raise (className =? "Firefox"))
> , ((modMask x .|. shiftMask, xK_b), runOrRaise "firefox" (className =? "Firefox"))

(Note that Firefox v3 and up have a class-name of "Firefox" and "Navigator";
lower versions use other classnames such as "Firefox-bin". Either choose the
appropriate one, or cover your bases by using instead something like
 @(className =? "Firefox" <||> className =? "Firefox-bin")@.)

For detailed instructions on editing your key bindings, see
"XMonad.Doc.Extending#Editing_key_bindings". -}

-- | 'action' is an executable to be run via 'spawn' if the Window cannot be found.
--   Presumably this executable is the same one that you were looking for.
runOrRaise :: String -> Query Bool -> X ()
runOrRaise action = raiseMaybe $ spawn action

-- | See 'raiseMaybe'. If the Window can't be found, quietly give up and do nothing.
raise :: Query Bool -> X ()
raise = raiseMaybe $ return ()

{- | 'raiseMaybe' queries all Windows based on a boolean provided by the
   user. Currently, there are three such useful booleans defined in
   XMonad.ManageHook: title, resource, className. Each one tests based pretty
   much as you would think. ManageHook also defines several operators, the most
   useful of which is (=?). So a useful test might be finding a Window whose
   class is Firefox. Firefox declares the class "Firefox", so you'd want to
   pass in a boolean like '(className =? "Firefox")'.

   If the boolean returns True on one or more windows, then XMonad will quickly
   make visible the first result. If no Window meets the criteria, then the
   first argument comes into play.

   The first argument is an arbitrary IO function which will be executed if the
   tests fail. This is what enables runOrRaise to use raiseMaybe: it simply runs
   the desired program if it isn't found. But you don't have to do that. Maybe
   you want to do nothing if the search fails (the definition of 'raise'), or
   maybe you want to write to a log file, or call some prompt function, or
   something crazy like that. This hook gives you that flexibility. You can do
   some cute things with this hook. Suppose you want to do the same thing for
   Mutt which you just did for Firefox - but Mutt runs inside a terminal window?
   No problem: you search for a terminal window calling itself 'mutt', and if
   there isn't you run a terminal with a command to run Mutt! Here's an example
   (borrowing "XMonad.Utils.Run"'s 'runInTerm'):

  > , ((modm, xK_m), raiseMaybe (runInTerm "-title mutt" "mutt") (title =? "mutt"))
-}
raiseMaybe :: X () -> Query Bool -> X ()
raiseMaybe f thatUserQuery = withWindowSet $ \s -> do
    maybeResult <- filterM (runQuery thatUserQuery) (W.allWindows s)
    case maybeResult of
      []     -> f
      (x:_)  -> focus x

-- | Given a function which gets us a String, we try to raise a window with that classname,
--   or we then interpret that String as a executable name.
raiseVar :: IO String -> X ()
raiseVar getvar = liftIO getvar >>= \var -> runOrRaise var (fmap (map toLower) className =? var)

-- | 'raiseBrowser' and 'raiseEditor' grab $BROWSER and $EDITOR respectively and they either
--   take you to the specified, or they try to run it. This is most useful if your variables are simple
--   and look like 'firefox' or 'emacs'.
raiseBrowser, raiseEditor :: X ()
raiseBrowser = raiseVar getBrowser
raiseEditor  = raiseVar getEditor
