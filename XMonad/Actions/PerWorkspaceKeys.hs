-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Actions.PerWorkspaceKeys
-- Description :  Define key-bindings on per-workspace basis.
-- Copyright   :  (c) Roman Cheplyaka, 2008
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  Roman Cheplyaka <roma@ro-che.info>
-- Stability   :  unstable
-- Portability :  unportable
--
-- Define key-bindings on per-workspace basis.
--
-----------------------------------------------------------------------------

module XMonad.Actions.PerWorkspaceKeys (
                                 -- * Usage
                                 -- $usage
                                 chooseAction,
                                 bindOn
                                ) where

import XMonad
import XMonad.StackSet as S

-- $usage
--
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- >  import XMonad.Actions.PerWorkspaceKeys
--
-- >   ,((0, xK_F2), bindOn [("1", spawn "rxvt"), ("2", spawn "xeyes"), ("", spawn "xmessage hello")])
--
-- For detailed instructions on editing your key bindings, see
-- "XMonad.Doc.Extending#Editing_key_bindings".

-- | Uses supplied function to decide which action to run depending on current workspace name.
chooseAction :: (String->X()) -> X()
chooseAction f = withWindowSet (f . S.currentTag)

-- | If current workspace is listed, run appropriate action (only the first match counts!)
-- If it isn't listed, then run default action (marked with empty string, \"\"), or do nothing if default isn't supplied.
bindOn :: [(String, X())] -> X()
bindOn bindings = chooseAction chooser where
    chooser ws = case lookup ws bindings of
        Just action -> action
        Nothing -> case lookup "" bindings of
            Just action -> action
            Nothing -> return ()
