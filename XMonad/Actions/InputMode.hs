-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Actions.InputMode
-- Copyright   :  (c) Bruno Dupuis <lisael@lisael.org>
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  Bruno Dupuis <lisael@lisael.org>
-- Stability   :  unstable
-- Portability :  unportable
--
-- A module that allows the user to create a so called imput mode, that is  a
-- persistent "XMonad.Actions.Submap".
--
-----------------------------------------------------------------------------

module XMonad.Actions.InputMode (
                             -- * Usage
                             -- $usage
                             inputMode
                            ) where
import XMonad hiding (keys)
import qualified Data.Map as M
import XMonad.Actions.Submap

{- $usage




First, import this module into your @~\/.xmonad\/xmonad.hs@:

> import XMonad.Actions.InputMode

Create a sub-mapping of keys. Example using "XMonad.Actions.FloatKeys":

> moveMode =
>     [ ((0, xK_h), withFocused (keysMoveWindow (-15,0) ) )
>     , ((0, xK_l), withFocused (keysMoveWindow (15,0) ) )
>     , ((0, xK_k), withFocused (keysMoveWindow (0,-15) ) )
>     , ((0, xK_j), withFocused (keysMoveWindow (0,15) ) )
>     , ((0,         xK_Tab), focusDown )
>     ]

Add a switch in your key bindings:

>    , ((modm,                 xK_g), inputMode moveMode)

So, you cat hit `modm+g` to activate the move mode and use
h, j, k, l to move a window around. Hit 'Escape' to restore your normal
bindings.

For detailed instructions on editing your key bindings, see
"XMonad.Doc.Extending#Editing_key_bindings".

-}

-- | Given a list of key bindings, return an action that temporay modifies
--   your bindings. Hit `Escape` to switch back to normal key bindings.
inputMode :: [( (KeyMask, KeySym), (X () ) )] -> X ()
inputMode xs = do
    submap . M.fromList $ modeMap where
        modeMap = [ ( binding , do x
                                   (submap . M.fromList $ modeMap) )
                    | ( binding , x ) <- xs
                  ] ++ [((0, xK_Escape), return ())]
