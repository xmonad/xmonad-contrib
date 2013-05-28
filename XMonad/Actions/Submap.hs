-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Actions.Submap
-- Copyright   :  (c) Jason Creighton <jcreigh@gmail.com>
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  Jason Creighton <jcreigh@gmail.com>
-- Stability   :  unstable
-- Portability :  unportable
--
-- A module that allows the user to create a sub-mapping of key bindings.
--
-----------------------------------------------------------------------------

module XMonad.Actions.Submap (
                             -- * Usage
                             -- $usage
                             submap,
                             submapDefault
                            ) where
import Data.Bits
import XMonad hiding (keys)
import qualified Data.Map as M
import Control.Monad.Fix (fix)

{- $usage




First, import this module into your @~\/.xmonad\/xmonad.hs@:

> import XMonad.Actions.Submap

Allows you to create a sub-mapping of keys. Example:

>    , ((modm, xK_a), submap . M.fromList $
>        [ ((0, xK_n),     spawn "mpc next")
>        , ((0, xK_p),     spawn "mpc prev")
>        , ((0, xK_z),     spawn "mpc random")
>        , ((0, xK_space), spawn "mpc toggle")
>        ])

So, for example, to run 'spawn \"mpc next\"', you would hit mod-a (to
trigger the submapping) and then 'n' to run that action. (0 means \"no
modifier\"). You are, of course, free to use any combination of
modifiers in the submapping. However, anyModifier will not work,
because that is a special value passed to XGrabKey() and not an actual
modifier.

For detailed instructions on editing your key bindings, see
"XMonad.Doc.Extending#Editing_key_bindings".

-}

-- | Given a 'Data.Map.Map' from key bindings to X () actions, return
--   an action which waits for a user keypress and executes the
--   corresponding action, or does nothing if the key is not found in
--   the map.
submap :: M.Map (KeyMask, KeySym) (X ()) -> X ()
submap keys = submapDefault (return ()) keys

-- | Like 'submap', but executes a default action if the key did not match.
submapDefault :: X () -> M.Map (KeyMask, KeySym) (X ()) -> X ()
submapDefault defAction keys = do
    XConf { theRoot = root, display = d } <- ask

    io $ grabKeyboard d root False grabModeAsync grabModeAsync currentTime

    (m, s) <- io $ allocaXEvent $ \p -> fix $ \nextkey -> do
        maskEvent d keyPressMask p
        KeyEvent { ev_keycode = code, ev_state = m } <- getEvent p
        keysym <- keycodeToKeysym d code 0
        if isModifierKey keysym
            then nextkey
            else return (m, keysym)
    -- Remove num lock mask and Xkb group state bits
    m' <- cleanMask $ m .&. ((1 `shiftL` 12) - 1)

    io $ ungrabKeyboard d currentTime

    maybe defAction id (M.lookup (m', s) keys)
