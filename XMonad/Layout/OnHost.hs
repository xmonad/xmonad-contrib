{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.OnHost
-- Description :  Use layouts and apply layout modifiers selectively, depending on the host.
-- Copyright   :  (c) Brandon S Allbery, Brent Yorgey
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  <allbery.b@gmail.com>
-- Stability   :  unstable
-- Portability :  unportable
--
-- Configure layouts on a per-host basis: use layouts and apply
-- layout modifiers selectively, depending on the host.  Heavily based on
-- "XMonad.Layout.PerWorkspace" by Brent Yorgey.
-----------------------------------------------------------------------------

module XMonad.Layout.OnHost (-- * Usage
                             -- $usage
                             OnHost
                            ,onHost
                            ,onHosts
                            ,modHost
                            ,modHosts
                            ) where

import           XMonad
import qualified XMonad.StackSet              as W
import           XMonad.Prelude

import           XMonad.Layout.LayoutModifier

import           Foreign                           (allocaArray0)
import           Foreign.C
import           System.Posix.Env                  (getEnv)

-- $usage
-- You can use this module by importing it into your @xmonad.hs@ file:
--
-- > import XMonad.Layout.OnHost
--
-- and modifying your 'layoutHook' as follows (for example):
--
-- > layoutHook = modHost "baz" m1 $            -- apply layout modifier m1 to all layouts on host "baz"
-- >              onHost "foo" l1 $             -- layout l1 will be used on host "foo".
-- >              onHosts ["bar","quux"] l2 $   -- layout l2 will be used on hosts "bar" and "quux".
-- >              l3                            -- layout l3 will be used on all other hosts.
--
-- Note that @l1@, @l2@, and @l3@ can be arbitrarily complicated
-- layouts, e.g. @(Full ||| smartBorders $ tabbed shrinkText
-- def ||| ...)@, and @m1@ can be any layout modifier, i.e. a
-- function of type @(l a -> ModifiedLayout lm l a)@.
--
-- In another scenario, suppose you wanted to have layouts A, B, and C
-- available on all hosts, except that on host foo you want
-- layout D instead of C.  You could do that as follows:
--
-- > layoutHook = A ||| B ||| onHost "foo" D C
--
-- Note that we rely on either @$HOST@ being set in the environment, or
-- <https://linux.die.net/man/2/gethostname gethostname> returning something
-- useful, as is true on most modern systems; if this is not the case for you,
-- you may want to use a wrapper around xmonad or perhaps use
-- 'System.Posix.Env.setEnv' (or 'putEnv') to set @$HOST@ in 'main'. If
-- neither of the two methods work, the module will behave as if the host name
-- never matches.
--
-- Also note that '$HOST' is usually a fully qualified domain name, not a short name.
-- If you use a short name, this code will try to truncate $HOST to match; this may
-- prove too magical, though, and may change in the future.

-- | Specify one layout to use on a particular host, and another
--   to use on all others.  The second layout can be another call to
--   'onHost', and so on.
onHost :: (LayoutClass l1 a, LayoutClass l2 a)
       => String -- ^ the name of the host to match
       -> l1 a   -- ^ layout to use on the matched host
       -> l2 a   -- ^ layout to use everywhere else
       -> OnHost l1 l2 a
onHost host = onHosts [host]

-- | Specify one layout to use on a particular set of hosts, and
--   another to use on all other hosts.
onHosts :: (LayoutClass l1 a, LayoutClass l2 a)
        => [String] -- ^ names of hosts to match
        -> l1 a     -- ^ layout to use on matched hosts
        -> l2 a     -- ^ layout to use everywhere else
        -> OnHost l1 l2 a
onHosts hosts = OnHost hosts False

-- | Specify a layout modifier to apply on a particular host; layouts
--   on all other hosts will remain unmodified.
modHost :: (LayoutClass l a)
        => String                          -- ^ name of the host to match
        -> (l a -> ModifiedLayout lm l a)  -- ^ the modifier to apply on the matching host
        -> l a                             -- ^ the base layout
        -> OnHost (ModifiedLayout lm l) l a
modHost host = modHosts [host]

-- | Specify a layout modifier to apply on a particular set of
--   hosts; layouts on all other hosts will remain
--   unmodified.
modHosts :: (LayoutClass l a)
         => [String]                        -- ^ names of the hosts to match
         -> (l a -> ModifiedLayout lm l a)  -- ^ the modifier to apply on the matching hosts
         -> l a                             -- ^ the base layout
         -> OnHost (ModifiedLayout lm l) l a
modHosts hosts f l = OnHost hosts False (f l) l

-- | Structure for representing a host-specific layout along with
--   a layout for all other hosts. We store the names of hosts
--   to be matched, and the two layouts. We save the layout choice in
--   the Bool, to be used to implement description.
data OnHost l1 l2 a = OnHost [String]
                             Bool
                             (l1 a)
                             (l2 a)
    deriving (Read, Show)

instance (LayoutClass l1 a, LayoutClass l2 a, Show a) => LayoutClass (OnHost l1 l2) a where
    runLayout (W.Workspace i p@(OnHost hosts _ lt lf) ms) r = do
      h <- io $ getEnv "HOST" <|> getHostName
      if maybe False (`elemFQDN` hosts) h
        then do (wrs, mlt') <- runLayout (W.Workspace i lt ms) r
                return (wrs, Just $ mkNewOnHostT p mlt')
        else do (wrs, mlt') <- runLayout (W.Workspace i lf ms) r
                return (wrs, Just $ mkNewOnHostF p mlt')

    handleMessage (OnHost hosts choice lt lf) m
        | choice    = handleMessage lt m >>= maybe (return Nothing) (\nt -> return . Just $ OnHost hosts choice nt lf)
        | otherwise = handleMessage lf m >>= maybe (return Nothing) (return . Just . OnHost hosts choice lt)

    description (OnHost _ True  l1 _) = description l1
    description (OnHost _ _     _ l2) = description l2

-- | Construct new OnHost values with possibly modified layouts.
mkNewOnHostT :: OnHost l1 l2 a -> Maybe (l1 a) -> OnHost l1 l2 a
mkNewOnHostT (OnHost hosts _ lt lf) mlt' =
  (\lt' -> OnHost hosts True lt' lf) $ fromMaybe lt mlt'

mkNewOnHostF :: OnHost l1 l2 a -> Maybe (l2 a) -> OnHost l1 l2 a
mkNewOnHostF (OnHost hosts _ lt lf) mlf' =
  OnHost hosts False lt $ fromMaybe lf mlf'

-- | 'Data.List.elem' except that if one side has a dot and the other doesn't, we truncate
--   the one that does at the dot.
elemFQDN           :: String -> [String] -> Bool
elemFQDN _  []     =  False
elemFQDN h0 (h:hs)
  | h0 `eqFQDN` h  =  True
  | otherwise      =  elemFQDN h0 hs

-- | String equality, possibly truncating one side at a dot.
eqFQDN :: String -> String -> Bool
eqFQDN a b
  | '.' `elem` a && '.' `elem` b =                    a ==                    b
  | '.' `elem` a                 = takeWhile (/= '.') a ==                    b
  |                 '.' `elem` b =                    a == takeWhile (/= '.') b
  | otherwise                    =                    a ==                    b

-----------------------------------------------------------------------
-- cbits

foreign import ccall "gethostname" gethostname :: CString -> CSize -> IO CInt

getHostName :: IO (Maybe String)
getHostName = allocaArray0 size $ \cstr -> do
  throwErrnoIfMinus1_ "getHostName" $ gethostname cstr (fromIntegral size)
  peekCString cstr <&> \case
    "" -> Nothing
    s  -> Just s
 where
  size = 256
