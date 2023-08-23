{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}

{- |
   Module       : XMonad.Layout.ConditionalLayout
   Description  : Conditionally apply layout and layout modifiers.
   Copyright    : (c) Ivan Malison   <IvanMalison@gmail.com>,
                      Tomas Janousek <tomi@nomi.cz>
   License      : BSD
   Maintainer   : Tony Zorman <soliditsallgood@mailbox.org>

   This module provides conditional variants of 'ModifiedLayout' and 'Choose',
   so that modifications (specific layouts) are only applied when a particular
   condition is met.
-}
module XMonad.Layout.ConditionalLayout (
  -- * Usage
  -- $usage

  -- * Combinators
  condChoose,
  conditional,

  -- * Building Conditions
  ModifierCondition (shouldApply),
) where

import XMonad hiding (hide)
import XMonad.Layout.LayoutModifier
import XMonad.Prelude
import XMonad.StackSet (Workspace)
import qualified XMonad.StackSet as W

{- $usage

You can use this module by importing it in your @xmonad.hs@

> import XMonad.Layout.ConditionalLayout

and writing an appropriate condition. Then, when 'conditional' is used,
a layout modifier will be applied whenever the condition is true.
Alternatively, 'condChoose' can decide which of two given layouts is to
be applied.

Defining a condition works by creating a new type, and making it an
instance of 'ModifierCondition'. For example, the following condition
checks whether there are a maximum of two windows on the current
workspace:

> import Data.List
>
> import XMonad
> import XMonad.Layout.ConditionalLayout
> import qualified XMonad.StackSet as W
>
> data IfMax2 = IfMax2 deriving (Read, Show)
>
> instance ModifierCondition IfMax2 where
>   shouldApply _ wsId = do
>     wins <- gets $ find ((wsId ==) . W.tag) . W.workspaces . windowset
>     let ws = W.integrate' . W.stack <$> wins
>     pure $ Just 2 >= (length <$> ws)

the @IfMax2@ type can now be used with the provided combinators. To
apply tabs, one would write

> import XMonad.Layout.Tabbed
>
> main :: IO ()
> main = xmonad $ def
>   { terminal   = "urxvt"
>   , layoutHook = conditional IfMax2 (addTabsAlways shrinkText def) $ layoutHook def
>   }

alternatively, to conditionally switch between two layouts:

>                  -- Full for workspaces with more than 4 windows, Tall otherwise.
>   , layoutHook = condChoose IfMax2 (Tall 1 (3/100) (1/2)) Full $ layoutHook def
-}

-- | Conditionally apply a layout modifier.
--
-- > conditional MyCond myModifier myLayout
conditional :: ModifierCondition c => c -> (l a -> ModifiedLayout m l a) -> (l a -> CondModifiedLayout c m l a)
conditional c ml = CondModifiedLayout True c . ml

-- | Conditionally choose between two layouts.
--
-- > condChoose MyCond myLayoutTrue myLayoutFalse
condChoose :: c -> l a -> r a -> CondChoose c l r a
condChoose c = CondChoose True c .: Choose CL

-- The reason that this must exist as a type class and a simple function will
-- not suffice is that 'ModifierCondition's are used as parameters to
-- 'CondModifiedLayout', which must implement 'Read' and 'Show' in order to
-- also implement 'LayoutModifier'. By defining a new type for condition, we
-- sidestep the issue that functions can not implement these type classes.

-- | A 'ModifierCondition' is a condition run in 'X' that takes a 'WorkspaceId'
-- as a parameter.
class (Read c, Show c) => ModifierCondition c where
  shouldApply :: c -> WorkspaceId -> X Bool

-- | 'ModifiedLayout' extended with a condition and its last evaluation result
-- (for methods that can't evaluate it).
data CondModifiedLayout c m l a = CondModifiedLayout Bool c (ModifiedLayout m l a)
  deriving (Read, Show)

-- | 'Choose' extended with a condition.
data CondChoose c l r a = CondChoose Bool c (Choose l r a)
  deriving (Read, Show)

instance (ModifierCondition c, LayoutModifier m a, LayoutClass l a, Typeable c, Typeable m)
  => LayoutClass (CondModifiedLayout c m l) a
 where
  runLayout :: (ModifierCondition c, LayoutModifier m a, LayoutClass l a, Typeable m, Typeable c)
            => Workspace WorkspaceId (CondModifiedLayout c m l a) a
            -> Rectangle
            -> X ([(a, Rectangle)], Maybe (CondModifiedLayout c m l a))
  runLayout (W.Workspace i cml@(CondModifiedLayout a c _) ms) r = do
    a' <- shouldApply c i
    cml' <- if a == a' then pure Nothing else Just . switch <$> hide cml
    fmap (<|> cml') <$> run (fromMaybe cml cml')
   where
    switch :: CondModifiedLayout c m l a -> CondModifiedLayout c m l a
    switch (CondModifiedLayout a' c' ml') = CondModifiedLayout (not a') c' ml'

    run :: CondModifiedLayout c m l a -> X ([(a, Rectangle)], Maybe (CondModifiedLayout c m l a))
    run (CondModifiedLayout True c' ml) =
      fmap (fmap (CondModifiedLayout True c')) <$> runLayout (W.Workspace i ml ms) r
    run (CondModifiedLayout False c' (ModifiedLayout m l)) =
      fmap (fmap (CondModifiedLayout False c' . ModifiedLayout m)) <$> runLayout (W.Workspace i l ms) r

  handleMessage :: CondModifiedLayout c m l a -> SomeMessage -> X (Maybe (CondModifiedLayout c m l a))
  handleMessage (CondModifiedLayout a c ml@(ModifiedLayout lm l)) m
    | Just ReleaseResources <- fromMessage m = fmap (CondModifiedLayout a c) <$> handleMessage ml m
    | a                                      = fmap (CondModifiedLayout a c) <$> handleMessage ml m
    | otherwise          = fmap (CondModifiedLayout a c . ModifiedLayout lm) <$> handleMessage l m

  description :: CondModifiedLayout c m l a -> String
  description (CondModifiedLayout a _ ml@(ModifiedLayout _ l))
    | a         = description ml
    | otherwise = description l

instance (ModifierCondition c, LayoutClass l a, LayoutClass r a, Typeable c)
  => LayoutClass (CondChoose c l r) a
 where
  runLayout :: (ModifierCondition c, LayoutClass l a, LayoutClass r a, Typeable c)
           => Workspace WorkspaceId (CondChoose c l r a) a
           -> Rectangle
           -> X ([(a, Rectangle)], Maybe (CondChoose c l r a))
  runLayout (W.Workspace i cl@(CondChoose b c _) ms) rect = do
    a <- shouldApply c i
    cl' <- if a == b then pure Nothing else Just . switch <$> hide cl
    fmap (<|> cl') <$> run (fromMaybe cl cl')
   where
    switch :: CondChoose c l r a -> CondChoose c l r a
    switch (CondChoose b' c' ch') = CondChoose (not b') c' ch'

    run :: CondChoose c l r a -> X ([(a, Rectangle)], Maybe (CondChoose c l r a))
    run (CondChoose preferLeft c' (Choose _ l r)) =
      fmap (fmap (CondChoose preferLeft c'))
        <$> runLayout (W.Workspace i (Choose (decideSide preferLeft) l r) ms) rect

  handleMessage :: CondChoose c l r a -> SomeMessage -> X (Maybe (CondChoose c l r a))
  handleMessage (CondChoose a c ch@(Choose _ l r)) m
    | Just ReleaseResources <- fromMessage m =
        fmap (CondChoose a c) <$> handleMessage ch m
    | otherwise =
        fmap (CondChoose a c) <$> handleMessage (Choose (decideSide a) l r) m

  description :: CondChoose c l r a -> String
  description (CondChoose a _ (Choose _ l r))
    | a         = description l
    | otherwise = description r

------------------------------------------------------------------------
-- Util

decideSide :: Bool -> CLR
decideSide = \case
  True  -> CL
  False -> CR

hide :: LayoutClass l a => l a -> X (l a)
hide x = fromMaybe x <$> handleMessage x (SomeMessage Hide)
