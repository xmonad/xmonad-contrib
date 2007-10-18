{-# -fglasgow-exts #-} -- For deriving Data/Typeable
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, PatternGuards #-}

-----------------------------------------------------------------------------
-- |
-- Module       : XMonadContrib.ToggleLayouts
-- Copyright    : (c) David Roundy <droundy@darcs.net>
-- License      : BSD
--
-- Maintainer   : David Roundy <droundy@darcs.net>
-- Stability    : unstable
-- Portability  : portable
--
-- A module for writing easy Layouts
-----------------------------------------------------------------------------

module XMonadContrib.ToggleLayouts (
    -- * Usage
    -- $usage
    toggleLayouts, ToggleLayout(..)
    ) where

import XMonad
import Operations ( LayoutMessages(Hide, ReleaseResources) )

-- $usage
-- Use toggleLayouts to toggle between two layouts.
-- import XMonadContrib.ToggleLayouts, and add to your layoutHook something like
-- > layoutHook = Layout $ toggleLayouts (noBorders Full) $ Select layouts
-- and a key binding like
-- >    , ((modMask .|. controlMask, xK_space), sendMessage ToggleLayout)

data ToggleLayouts lt lf a = ToggleLayouts Bool (lt a) (lf a) deriving (Read,Show)
data ToggleLayout = ToggleLayout deriving (Read,Show,Typeable)
instance Message ToggleLayout

toggleLayouts :: (LayoutClass lt a, LayoutClass lf a) => lt a -> lf a -> ToggleLayouts lt lf a
toggleLayouts = ToggleLayouts False

instance (LayoutClass lt a, LayoutClass lf a) => LayoutClass (ToggleLayouts lt lf) a where
    doLayout (ToggleLayouts True lt lf) r s = do (ws,mlt') <- doLayout lt r s
                                                 return (ws,fmap (\lt' -> ToggleLayouts True lt' lf) mlt')
    doLayout (ToggleLayouts False lt lf) r s = do (ws,mlf') <- doLayout lf r s
                                                  return (ws,fmap (\lf' -> ToggleLayouts False lt lf') mlf')
    description (ToggleLayouts True lt _) = description lt
    description (ToggleLayouts False _ lf) = description lf
    handleMessage (ToggleLayouts bool lt lf) m
        | Just ReleaseResources <- fromMessage m =
                                   do mlf' <- handleMessage lf m
                                      mlt' <- handleMessage lt m
                                      return $ case (mlt',mlf') of
                                          (Nothing ,Nothing ) -> Nothing
                                          (Just lt',Nothing ) -> Just $ ToggleLayouts bool lt' lf
                                          (Nothing ,Just lf') -> Just $ ToggleLayouts bool lt lf'
                                          (Just lt',Just lf') -> Just $ ToggleLayouts bool lt' lf'
    handleMessage (ToggleLayouts True lt lf) m
        | Just ToggleLayout <- fromMessage m = do mlt' <- handleMessage lt (SomeMessage Hide)
                                                  let lt' = maybe lt id mlt'
                                                  return $ Just $ ToggleLayouts False lt' lf
        | otherwise = do mlt' <- handleMessage lt m
                         return $ fmap (\lt' -> ToggleLayouts True lt' lf) mlt'
    handleMessage (ToggleLayouts False lt lf) m
        | Just ToggleLayout <- fromMessage m = do mlf' <- handleMessage lf (SomeMessage Hide)
                                                  let lf' = maybe lf id mlf'
                                                  return $ Just $ ToggleLayouts True lt lf'
        | otherwise = do mlf' <- handleMessage lf m
                         return $ fmap (\lf' -> ToggleLayouts False lt lf') mlf'
