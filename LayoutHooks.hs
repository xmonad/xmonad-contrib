-----------------------------------------------------------------------------
-- |
-- Module       : XMonadContrib.LayoutHooks
-- Copyright    : (c) Stefan O'Rear <stefanor@cox.net>
-- License      : BSD
--
-- Maintainer   : Stefan O'Rear <stefanor@cox.net>
-- Stability    : unstable
-- Portability  : portable
--
-- General layout-level hooks.
-----------------------------------------------------------------------------

module XMonadContrib.LayoutHooks ( addLayoutMessageHook ) where

import qualified Data.Map as M  ( adjust  )
import Control.Arrow            ( first   )
import Control.Monad.State      ( modify  )

import XMonad
import qualified StackSet as W

install :: (SomeMessage -> X Bool) -> Layout a -> Layout a
install hk lay = lay{ modifyLayout = mod' }
  where
    mod' msg = do reinst  <- hk msg
                  nlay    <- modifyLayout lay msg

                  return $ cond_reinst reinst nlay

    -- no need to make anything change
    cond_reinst True   Nothing      = Nothing
    -- reinstall
    cond_reinst True   (Just nlay)  = Just (install hk nlay)
    -- restore inner layout
    cond_reinst False  Nothing      = Just lay
    -- let it rot
    cond_reinst False  (Just nlay)  = Just nlay

-- Return True each time you want the hook reinstalled
addLayoutMessageHook     :: (SomeMessage -> X Bool) -> X ()
addLayoutMessageHook hk  = modify $ \ s ->
    let nr = W.tag . W.workspace . W.current . windowset $ s
      in s { layouts = M.adjust (first $ install hk) nr (layouts s) }
