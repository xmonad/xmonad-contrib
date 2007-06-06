{-
Allows you to create a sub-mapping of keys. Example:

    , ((modMask, xK_a), submap . M.fromList $
        [ ((0, xK_n),     spawn "mpc next")
        , ((0, xK_p),     spawn "mpc prev")
        , ((0, xK_z),     spawn "mpc random")
        , ((0, xK_space), spawn "mpc toggle")
        ])

So, for example, to run 'spawn "mpc next"', you would hit mod-a (to trigger the
submapping) and then 'n' to run that action. (0 means "no modifier"). You are,
of course, free to use any combination of modifiers in the submapping. However,
anyModifier will not work, because that is a special value passed to XGrabKey()
and not an actual modifier.
-}

module XMonadContrib.Submap where

import Control.Monad.Reader

import XMonad
import Operations (cleanMask)
import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras
import qualified Data.Map as M

submap :: M.Map (KeyMask, KeySym) (X ()) -> X ()
submap keys = do
    XConf { theRoot = root, display = d } <- ask

    io $ grabKeyboard d root False grabModeAsync grabModeAsync currentTime

    keyspec <- io $ allocaXEvent $ \p -> fix $ \nextkey -> do
        maskEvent d keyPressMask p
        KeyEvent { ev_keycode = code, ev_state = m } <- getEvent p
        keysym <- keycodeToKeysym d code 0
        if isModifierKey keysym
            then nextkey
            else return (cleanMask m, keysym)

    io $ ungrabKeyboard d currentTime

    whenJust (M.lookup keyspec keys) id
