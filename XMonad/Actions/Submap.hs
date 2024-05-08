-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Actions.Submap
-- Description :  Create a sub-mapping of key bindings.
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
                             visualSubmap,
                             visualSubmapSorted,
                             submapDefault,
                             submapDefaultWithKey,

                             -- * Utilities
                             subName,
                            ) where
import Data.Bits
import qualified Data.Map as M
import XMonad hiding (keys)
import XMonad.Prelude (fix, fromMaybe, keyToString, cleanKeyMask)
import XMonad.Util.XUtils

{- $usage

First, import this module into your @xmonad.hs@:

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
<https://xmonad.org/TUTORIAL.html#customizing-xmonad the tutorial>.

-}

-- | Given a 'Data.Map.Map' from key bindings to X () actions, return
--   an action which waits for a user keypress and executes the
--   corresponding action, or does nothing if the key is not found in
--   the map.
submap :: M.Map (KeyMask, KeySym) (X ()) -> X ()
submap = submapDefault (return ())

-- | Like 'submap', but visualise the relevant options.
--
-- ==== __Example__
--
-- > import qualified Data.Map as Map
-- > import XMonad.Actions.Submap
-- >
-- > gotoLayout :: [(String, X ())]   -- for use with EZConfig
-- > gotoLayout =  -- assumes you have a layout named "Tall" and one named "Full".
-- >   [("M-l", visualSubmap def $ Map.fromList $ map (\(k, s, a) -> ((0, k), (s, a)))
-- >              [ (xK_t, "Tall", switchToLayout "Tall")     -- "M-l t" switches to "Tall"
-- >              , (xK_r, "Full", switchToLayout "Full")     -- "M-l r" switches to "full"
-- >              ])]
--
-- One could alternatively also write @gotoLayout@ as
--
-- > gotoLayout = [("M-l", visualSubmap def $ Map.fromList $
-- >                         [ ((0, xK_t), subName "Tall" $ switchToLayout "Tall")
-- >                         , ((0, xK_r), subName "Full" $ switchToLayout "Full")
-- >                         ])]
visualSubmap :: WindowConfig -- ^ The config for the spawned window.
             -> M.Map (KeyMask, KeySym) (String, X ())
                             -- ^ A map @keybinding -> (description, action)@.
             -> X ()
visualSubmap = visualSubmapSorted id

-- | Like 'visualSubmap', but is able to sort the descriptions.
-- For example,
--
-- > import Data.Ord (comparing, Down)
-- >
-- > visualSubmapSorted (sortBy (comparing Down)) def
--
-- would sort the @(key, description)@ pairs by their keys in descending
-- order.
visualSubmapSorted :: ([((KeyMask, KeySym), String)] -> [((KeyMask, KeySym), String)])
                             -- ^ A function to resort the descriptions
             -> WindowConfig -- ^ The config for the spawned window.
             -> M.Map (KeyMask, KeySym) (String, X ())
                             -- ^ A map @keybinding -> (description, action)@.
             -> X ()
visualSubmapSorted sorted wc keys =
    withSimpleWindow wc descriptions waitForKeyPress >>= \(m', s) ->
        maybe (pure ()) snd (M.lookup (m', s) keys)
  where
    descriptions :: [String]
    descriptions =
        map (\(key, desc) -> keyToString key <> ": " <> desc)
            . sorted
            $ zip (M.keys keys) (map fst (M.elems keys))

-- | Give a name to an action.
subName :: String -> X () -> (String, X ())
subName = (,)

-- | Like 'submap', but executes a default action if the key did not match.
submapDefault :: X () -> M.Map (KeyMask, KeySym) (X ()) -> X ()
submapDefault = submapDefaultWithKey . const

-- | Like 'submapDefault', but sends the unmatched key to the default
-- action as argument.
submapDefaultWithKey :: ((KeyMask, KeySym) -> X ())
                     -> M.Map (KeyMask, KeySym) (X ())
                     -> X ()
submapDefaultWithKey defAction keys = waitForKeyPress >>=
    \(m', s) -> fromMaybe (defAction (m', s)) (M.lookup (m', s) keys)

-----------------------------------------------------------------------
-- Internal stuff

waitForKeyPress :: X (KeyMask, KeySym)
waitForKeyPress = do
    XConf{ theRoot = root, display = dpy } <- ask

    io $ do grabKeyboard dpy root False grabModeAsync grabModeAsync currentTime
            grabPointer dpy root False buttonPressMask grabModeAsync grabModeAsync
                        none none currentTime

    (m, s) <- io $ allocaXEvent $ \p -> fix $ \nextkey -> do
        maskEvent dpy (keyPressMask .|. buttonPressMask) p
        ev <- getEvent p
        case ev of
          KeyEvent { ev_keycode = code, ev_state = m } -> do
            keysym <- keycodeToKeysym dpy code 0
            if isModifierKey keysym
                then nextkey
                else return (m, keysym)
          _ -> return (0, 0)
    m' <- cleanKeyMask <*> pure m
    io $ do ungrabPointer dpy currentTime
            ungrabKeyboard dpy currentTime
            sync dpy False
    pure (m', s)
