-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Actions.PerWindowKeys
-- Description :  Define key-bindings on a per-window basis.
-- Copyright   :  (c) Wilson Sales, 2019
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  Wilson Sales <spoonm@spoonm.org>
-- Stability   :  unstable
-- Portability :  unportable
--
-- Define key-bindings on a per-window basis.
--
-----------------------------------------------------------------------------

module XMonad.Actions.PerWindowKeys (
                                    -- * Usage
                                    -- $usage
                                    bindAll,
                                    bindFirst
                                   ) where

import XMonad

-- $usage
--
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- >  import XMonad.Actions.PerWindowKeys
--
-- >   ,((0, xK_F2), bindFirst [(className =? "firefox", spawn "dmenu"), (isFloat, withFocused $ windows . W.sink)])
--
-- >   ,((0, xK_F3), bindAll [(isDialog, kill), (pure True, doSomething)])
--
-- If you want an action that will always run, but also want to do something for
-- other queries, you can use @'bindAll' [(query1, action1), ..., (pure True,
-- alwaysDoThisAction)]@.
--
-- Similarly, if you want a default action to be run if all the others failed,
-- you can use @'bindFirst' [(query1, action1), ..., (pure True,
-- doThisIfTheOthersFail)]@.
--
-- For detailed instructions on editing your key bindings, see
-- "XMonad.Doc.Extending#Editing_key_bindings".

-- | Run an action if a Query holds true. Doesn't stop at the first one that
-- does, however, and could potentially run all actions.
bindAll :: [(Query Bool, X ())] -> X ()
bindAll = mapM_ choose where
  choose (mh,action) = withFocused $ \w -> whenX (runQuery mh w) action

-- | Run the action paired with the first Query that holds true.
bindFirst :: [(Query Bool, X ())] -> X ()
bindFirst = withFocused . chooseOne

chooseOne :: [(Query Bool, X ())] -> Window -> X ()
chooseOne [] _ = return ()
chooseOne ((mh,a):bs) w = do
  c <- runQuery mh w
  if c then a
       else chooseOne bs w
