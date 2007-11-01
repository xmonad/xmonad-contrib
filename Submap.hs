-----------------------------------------------------------------------------
-- |
-- Module      :  XMonadContrib.Submap
-- Copyright   :  (c) Jason Creighton <jcreigh@gmail.com>
-- License     :  BSD3-style (see LICENSE)
-- 
-- Maintainer  :  Jason Creighton <jcreigh@gmail.com>
-- Stability   :  unstable
-- Portability :  unportable
--
-- A module that allows the user to create a sub-mapping of keys bindings.
--
-----------------------------------------------------------------------------

module XMonadContrib.Submap (
                             -- * Usage
                             -- $usage
                             submap
                            ) where

import Control.Monad.Reader

import XMonad
import XMonad.Operations (cleanMask)
import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras
import qualified Data.Map as M

{- $usage
Allows you to create a sub-mapping of keys. Example:

>    , ((modMask, xK_a), submap . M.fromList $
>        [ ((0, xK_n),     spawn "mpc next")
>        , ((0, xK_p),     spawn "mpc prev")
>        , ((0, xK_z),     spawn "mpc random")
>        , ((0, xK_space), spawn "mpc toggle")
>        ])

So, for example, to run 'spawn \"mpc next\"', you would hit mod-a (to trigger the
submapping) and then 'n' to run that action. (0 means \"no modifier\"). You are,
of course, free to use any combination of modifiers in the submapping. However,
anyModifier will not work, because that is a special value passed to XGrabKey()
and not an actual modifier.
-}

-- %import XMonadContrib.Submap
-- %keybind , ((modMask, xK_a), submap . M.fromList $
-- %keybind     [ ((0, xK_n),     spawn "mpc next")
-- %keybind     , ((0, xK_p),     spawn "mpc prev")
-- %keybind     , ((0, xK_z),     spawn "mpc random")
-- %keybind     , ((0, xK_space), spawn "mpc toggle")
-- %keybind     ])

submap :: M.Map (KeyMask, KeySym) (X ()) -> X ()
submap keys = do
    XConf { theRoot = root, display = d } <- ask

    io $ grabKeyboard d root False grabModeAsync grabModeAsync currentTime

    (m, s) <- io $ allocaXEvent $ \p -> fix $ \nextkey -> do
        maskEvent d keyPressMask p
        KeyEvent { ev_keycode = code, ev_state = m } <- getEvent p
        keysym <- keycodeToKeysym d code 0
        if isModifierKey keysym
            then nextkey
            else return (m, keysym)

    io $ ungrabKeyboard d currentTime

    m' <- cleanMask m
    whenJust (M.lookup (m', s) keys) id
