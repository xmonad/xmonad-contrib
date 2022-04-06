-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Actions.PerLayouteKeys
-- Description :  Define key-bindings on per-layout basis.
-- Copyright   :  (c) brandon s allbery kf8nh 2022, Roman Cheplyaka, 2008
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  brandon s allbery kf8ng <allbery.b@gmail.com>
-- Stability   :  unstable
-- Portability :  unportable
--
-- Define key-bindings on per-layout basis.
--
-----------------------------------------------------------------------------

module XMonad.Actions.PerLayoutKeys (
                                 -- * Usage
                                 -- $usage
                                 chooseActionByLayout,
                                 bindByLayout
                                ) where

import XMonad
import XMonad.StackSet as S

-- $usage
--
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- >  import XMonad.Actions.PerLayoutKeys
--
-- >   ,((0, xK_F2), bindByLayout [("Tall", spawn "rxvt"), ("Mirror Tall", spawn "xeyes"), ("", spawn "xmessage hello")])
--
-- For detailed instructions on editing your key bindings, see
-- "XMonad.Doc.Extending#Editing_key_bindings".

-- | Uses supplied function to decide which action to run depending on current layout name.
chooseActionByLayout :: (String->X()) -> X()
chooseActionByLayout f = withWindowSet (f . description . S.layout. S.workspace . S.current)

-- | If current layout is listed, run appropriate action (only the first match counts!)
-- If it isn't listed, then run default action (marked with empty string, \"\"), or do nothing if default isn't supplied.
bindByLayout :: [(String, X())] -> X()
bindByLayout bindings = chooseActionByLayout chooser where
    chooser l = case lookup l bindings of
        Just action -> action
        Nothing -> case lookup "" bindings of
            Just action -> action
            Nothing -> return ()
