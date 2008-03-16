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
remember where you left it or whether you still have one running.
-}

module XMonad.Actions.WindowGo (
                 -- * Usage
                 -- $usage
                 raise,
                 runOrRaise,
                 raiseMaybe,
                 module XMonad.ManageHook
                ) where

import XMonad (Query(), X(), withWindowSet, spawn, runQuery, focus)
import Control.Monad (filterM)
import qualified XMonad.StackSet as W (allWindows)
import XMonad.ManageHook

{- $usage

Import the module into your @~\/.xmonad\/xmonad.hs@:

> import XMonad.Actions.WindowGo

and define appropriate key bindings:

> , ((modMask x .|. shiftMask, xK_g), raise (className =? "Firefox"))
> , ((modMask x .|. shiftMask, xK_b), runOrRaise "firefox" (className =? "Firefox"))

(Note that Firefox v3 and up have a class-name of "Firefox" and "Navigator";
lower versions use other classnames such as "Firefox-bin"
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
