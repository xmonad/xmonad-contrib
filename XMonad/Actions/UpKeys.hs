{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
{- |
   Module      : XMonad.Actions.UpKeys
   Description : Bind an action to the release of a key
   Copyright   : (c) Tony Zorman, 2024
   License     : BSD-3
   Maintainer  : Tony Zorman <soliditsallgood@mailbox.org>

A combinator for binding an action to the release of a key. This can be
useful for hold-type buttons, where the press of a key engages some
functionality, and its release… releases it again.
-}
module XMonad.Actions.UpKeys
  ( -- * Usage
    -- $usage
    useUpKeys,
    UpKeysConfig (..),
    ezUpKeys,
  )
where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import XMonad
import XMonad.Prelude
import XMonad.Util.EZConfig (mkKeymap)
import qualified XMonad.Util.ExtensibleConf as XC

{- $usage
You can use this module with the following in your @xmonad.hs@:

> import XMonad.Actions.UpKeys

Next, define the keys and actions you want to have happen on the release
of a key:

> myUpKeys = ezUpKeys $
>   [ ("M-z", myAction)
>   , ("M-a", myAction2)
>   ]

All that's left is to plug this definition into the 'useUpKeys'
combinator that this module provides:

> main :: IO ()
> main = xmonad
>      . useUpKeys (def{ grabKeys = True, upKeys = myUpKeys })
>      $ myConfig

Note the presence of @'grabKeys' = True@; this is for situations where
you don't have any of these keys bound to do something upon pressing
them; i.e., you use them solely for their release actions. If you want
something to happen in both cases, remove that part (@'grabKeys' =
False@ is the default) and bind the keys to actions as you normally
would.

==== __Examples__

As an extended example, consider the case where you want all of your
docks (e.g., status bar) to "pop up" when you press the super key, and
then vanish again once that keys is released.

Since docks are not generally part of XMonad's window-set—otherwise, we
would have to manage them—we first need a way to access and manipulate
all docks.

> onAllDocks :: (Display -> Window -> IO ()) -> X ()
> onAllDocks act = withDisplay \dpy -> do
>   rootw <- asks theRoot
>   (_, _, wins) <- io $ queryTree dpy rootw
>   traverse_ (io . act dpy) =<< filterM (runQuery checkDock) wins

This is also the place where one could filter for just status bar,
trayer, and so on.

Now we have to decide what kinds of keys we want to watch out for. Since
you most likely use left super as your modifier key, this is a little
bit more complicated than for other keys, as you will most likely see
the key both as a @KeyMask@, as well as a @KeySym@. One could think a
bit and probably come up with an elegant solution for this—or one could
grab all possible key combinations by brute-force!

> dockKeys :: X () -> [((KeyMask, KeySym), X ())]
> dockKeys act = map (actKey . foldr1 (.|.)) . combinations $ keyMasks
>  where
>   actKey :: KeyMask -> ((KeyMask, KeySym), X ())
>   actKey mask = ((mask, xK_Super_L), act)
>
>   keyMasks :: [KeyMask]
>   keyMasks = [ noModMask, shiftMask, lockMask, controlMask, mod1Mask, mod2Mask, mod3Mask, mod4Mask, mod5Mask ]
>
>   -- Return all combinations of a sequence of values.
>   combinations :: [a] -> [[a]]
>   combinations xs = concat [combs i xs | i <- [1 .. length xs]]
>    where
>     combs 0 _      = [[]]
>     combs _ []     = []
>     combs n (x:xs) = map (x:) (combs (n-1) xs) <> combs n xs

Given some action, like lowering or raising the window, we generate all
possible combinations of modifiers that may be pressed with the super
key. This is a good time to say that this is just for demonstrative
purposes, btw—please don't actually do this.

All that's left is to plug everything into the machinery of this module,
and we're done!

> import qualified Data.Map.Strict as Map
>
> main :: IO ()
> main = xmonad
>      . … -- other combinators
>      . useUpKeys (def { upKeys = Map.fromList $ dockKeys (onAllDocks lowerWindow) })
>      $ myConfig `additionalKeys` dockKeys (onAllDocks raiseWindow)
>
> myConfig = …
-}

data UpKeysConfig = UpKeysConfig
  { -- | Whether to grab all keys that are not already grabbed.
    grabKeys :: !Bool
    -- | The keys themselves.
  , upKeys :: !(Map (KeyMask, KeySym) (X ()))
  }

-- | The default 'UpKeysConfig'; keys are not grabbed, and no upkeys are
-- specified.
instance Default UpKeysConfig where
  def :: UpKeysConfig
  def = UpKeysConfig { grabKeys = False, upKeys = mempty }

instance Semigroup UpKeysConfig where
  (<>) :: UpKeysConfig -> UpKeysConfig -> UpKeysConfig
  UpKeysConfig g u <> UpKeysConfig g' u' = UpKeysConfig (g && g') (u <> u')

-- | Bind actions to keys upon their release.
useUpKeys :: UpKeysConfig -> (XConfig l -> XConfig l)
useUpKeys upKeysConf = flip XC.once upKeysConf \conf -> conf
  { handleEventHook = handleEventHook conf <> (\e -> handleKeyUp e $> All True)
  , startupHook     = startupHook     conf <> when (grabKeys upKeysConf) grabUpKeys
  }
 where
  grabUpKeys :: X ()
  grabUpKeys = do
    XConf{ display = dpy, theRoot = rootw } <- ask
    realKeys <- maybe mempty upKeys <$> XC.ask @X @UpKeysConfig
    let grab :: (KeyMask, KeyCode) -> X ()
        grab (km, kc) = io $ grabKey dpy kc km rootw True grabModeAsync grabModeAsync
    traverse_ grab =<< mkGrabs (Map.keys realKeys)

-- | Parse the given EZConfig-style keys into the internal keymap
-- representation.
--
-- This is just 'mkKeymap' with a better name.
ezUpKeys :: XConfig l -> [(String, X ())] -> Map (KeyMask, KeySym) (X ())
ezUpKeys = mkKeymap

-- | A handler for key-up events.
handleKeyUp :: Event -> X ()
handleKeyUp KeyEvent{ ev_event_type, ev_state, ev_keycode }
  | ev_event_type == keyRelease = withDisplay \dpy -> do
      s   <- io $ keycodeToKeysym dpy ev_keycode 0
      cln <- cleanMask ev_state
      ks  <- maybe mempty upKeys <$> XC.ask @X @UpKeysConfig
      userCodeDef () $ whenJust (ks Map.!? (cln, s)) id
handleKeyUp _ = pure ()
