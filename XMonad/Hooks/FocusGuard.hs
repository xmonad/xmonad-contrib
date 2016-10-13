-----------------------------------------------------------------------------
-- |
-- Module       : XMonad.Hooks.FocusGuard
-- Copyright    : (c) 2016 Felix Hirn <felix.hirn@wyvernscale.com>
-- License      : BSD
--
-- Maintainer   : Felix Hirn <felix.hirn@wyvernscale.com>
-- Stability    : unstable
-- Portability  : unportable
--
-- Filters out certain events by certain windows to prevent them from
-- stealing focus. Especially useful when using some variation of
-- 'XMonad.Hooks.EwmhDesktop' 's hooks. Note that this does not protect
-- against the unholy art of cursor position manipulation (mainly seen
-- in video games).
-----------------------------------------------------------------------------

module XMonad.Hooks.FocusGuard
    ( -- * Usage
      -- $usage
      guardAgainst
    , guardHook
    ) where

import Data.Functor
import Data.Monoid

import XMonad


-- $usage
-- The easiest way to include this into your @xmonad.hs@ is:
--
-- > import XMonad.Hooks.FocusGuard
-- >
-- > offenders :: [Query Bool]
-- > offenders = [ className =? "Steam"
-- >             , className =? "steam"
-- >             , className =? "Linphone"
-- >             , className =? "linphone"
-- >             -- ...
-- >             ]
-- >
-- > main :: IO ()
-- > main = xmonad $ guardAgainst offenders $ myConfig
--
-- When including other global transformers, such as
-- `XMonad.Hooks.EwmhDesktops.ewmh`, make sure that `guardAgainst` is the
-- leftmost one in the chain, as it can only apply to hooks declared before
-- it is applied.
--
-- To figure out the `className` of a window, use the @xprop@ utility.



-- | Adds `guardHook` to an existing config. Easiest way to make sure that it
--   catches events for /all/ custom hooks.
guardAgainst :: [Query Bool] -> XConfig a -> XConfig a
guardAgainst list conf = conf { handleEventHook = guardHook list
                                                $ handleEventHook conf
                              }



-- | Blocks events of type @_NET_ACTIVE_WINDOW@ for windows matching any of the
--   given queries from entering the given event hook.
guardHook :: [Query Bool] -> (Event -> X All) -> Event -> X All
guardHook list hook ev@ClientMessageEvent
                            { ev_window       = window
                            , ev_message_type = mtype
                            } = do

    actWind <- getAtom "_NET_ACTIVE_WINDOW"

    if mtype /= actWind
       then hook ev
       else do
            b <- or <$> mapM (`runQuery` window) list

            if b
               then return $ All False
               else hook ev

guardHook _ hook ev = hook ev
