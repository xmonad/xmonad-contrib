{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, PatternGuards #-}

-----------------------------------------------------------------------------
-- |
-- Module       : XMonad.Layout.ToggleLayouts
-- Description :  A module to toggle between two layouts.
-- Copyright    : (c) David Roundy <droundy@darcs.net>
-- License      : BSD
--
-- Maintainer   : none
-- Stability    : unstable
-- Portability  : portable
--
-- A module to toggle between two layouts.
-----------------------------------------------------------------------------

module XMonad.Layout.ToggleLayouts (
    -- * Usage
    -- $usage
    toggleLayouts, ToggleLayout(..), ToggleLayouts
    ) where

import XMonad
import XMonad.Prelude (fromMaybe)
import XMonad.StackSet (Workspace (..))

-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Layout.ToggleLayouts
--
-- Then edit your @layoutHook@ by adding the ToggleLayouts layout:
--
-- > myLayout = toggleLayouts Full (Tall 1 (3/100) (1/2)) ||| etc..
-- > main = xmonad def { layoutHook = myLayout }
--
-- For more detailed instructions on editing the layoutHook see:
--
-- "XMonad.Doc.Extending#Editing_the_layout_hook"
--
-- To toggle between layouts add a key binding like
--
-- >    , ((modm .|. controlMask, xK_space), sendMessage ToggleLayout)
--
-- or a key binding like
--
-- >    , ((modm .|. controlMask, xK_space), sendMessage (Toggle "Full"))
--
-- For detailed instruction on editing the key binding see:
--
-- "XMonad.Doc.Extending#Editing_key_bindings".

data ToggleLayouts lt lf a = ToggleLayouts Bool (lt a) (lf a) deriving (Read,Show)
data ToggleLayout = ToggleLayout | Toggle String deriving (Read,Show)
instance Message ToggleLayout

toggleLayouts :: (LayoutClass lt a, LayoutClass lf a) => lt a -> lf a -> ToggleLayouts lt lf a
toggleLayouts = ToggleLayouts False

instance (LayoutClass lt a, LayoutClass lf a) => LayoutClass (ToggleLayouts lt lf) a where
    runLayout (Workspace i (ToggleLayouts True lt lf) ms) r = do (ws,mlt') <- runLayout (Workspace i lt ms) r
                                                                 return (ws,fmap (\lt' -> ToggleLayouts True lt' lf) mlt')

    runLayout (Workspace i (ToggleLayouts False lt lf) ms) r = do (ws,mlf') <- runLayout (Workspace i lf ms) r
                                                                  return (ws,fmap (ToggleLayouts False lt) mlf')
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
                                                  let lt' = fromMaybe lt mlt'
                                                  return $ Just $ ToggleLayouts False lt' lf
        | Just (Toggle d) <- fromMessage m,
          d == description lt || d == description lf =
              do mlt' <- handleMessage lt (SomeMessage Hide)
                 let lt' = fromMaybe lt mlt'
                 return $ Just $ ToggleLayouts False lt' lf
        | otherwise = do mlt' <- handleMessage lt m
                         return $ fmap (\lt' -> ToggleLayouts True lt' lf) mlt'
    handleMessage (ToggleLayouts False lt lf) m
        | Just ToggleLayout <- fromMessage m = do mlf' <- handleMessage lf (SomeMessage Hide)
                                                  let lf' = fromMaybe lf mlf'
                                                  return $ Just $ ToggleLayouts True lt lf'
        | Just (Toggle d) <- fromMessage m,
          d == description lt || d == description lf =
              do mlf' <- handleMessage lf (SomeMessage Hide)
                 let lf' = fromMaybe lf mlf'
                 return $ Just $ ToggleLayouts True lt lf'
        | otherwise = do mlf' <- handleMessage lf m
                         return $ fmap (ToggleLayouts False lt) mlf'
