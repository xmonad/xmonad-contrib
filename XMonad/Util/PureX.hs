{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleContexts #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Util.PureX
-- Copyright   :  L. S. Leary 2018
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  L. S. Leary
-- Stability   :  unstable
-- Portability :  not portable
--
-- Unlike the opaque @IO@ actions that @X@ actions can wrap, regular reads from
-- the 'XConf' and modifications to the 'XState' are fundamentally pure—contrary
-- to the current treatment of such actions in most xmonad code. Pure
-- modifications to the 'WindowSet' can be readily composed, but due to the need
-- for those modifications to be properly handled by 'windows', other pure
-- changes to the @XState@ cannot be interleaved with those changes to the
-- @WindowSet@ without superfluous refreshes, hence breaking composability.
--
-- This module aims to rectify that situation by drawing attention to it and
-- providing 'PureX': a pure type with the same monadic interface to state as
-- @X@. The 'XLike' typeclass enables writing actions generic over the two
-- monads; if pure, existing @X@ actions can be generalised with only a change
-- to the type signature. Various other utilities are provided, in particular
-- the 'defile' function which is needed by end-users.
--
-----------------------------------------------------------------------------

-- --< Imports & Exports >-- {{{

module XMonad.Util.PureX (
  -- * Usage
  -- $Usage
  PureX, XLike(..), defile,
  windowBracket', handlingRefresh,
  runPureX, toXLike,
  -- * Utility
  -- ** Generalised when* functions
  when', whenM', whenJust',
  -- ** Infix operators
  (<?), (&>),
  -- ** @WindowSet@ operations
  withWindowSet', withFocii,
  modify'', modifyWindowSet',
  getStack, putStack, peek,
  focusWindow, focusNth,
  view, greedyView, invisiView,
  shift, curScreen, curWorkspace,
  curTag, curScreenId,
) where

-- xmonad
import XMonad
import qualified XMonad.StackSet as W
import qualified XMonad.Actions.FocusNth

-- mtl
import Control.Monad.State
import Control.Monad.Reader

-- base
import Data.Semigroup (Any(..))
import Control.Applicative (liftA2)

-- }}}

-- --< Usage >-- {{{

-- $Usage
--
-- The suggested pattern of usage for this module is to write composable, pure
-- actions as @XLike m => m Any@ or @PureX Any@ values, where the encapsulated
-- @Any@ value encodes whether or not a refresh is needed to properly institute
-- changes. These values can then be combined monoidally (i.e. with '<>' AKA
-- '<+>') or with operators such as '<*', '*>', '<?' and '&>' to build seamless
-- new actions. The end user can run and handle the effects of the pure actions
-- in the @X@ monad by applying the @defile@ function, which you may want to
-- re-export. Alternatively, if an action does not make stackset changes that
-- need to be handled by @windows@, it can be written with as an
-- @XLike m => m ()@ and used directly.
--
-- Unfortunately since layouts must handle messages in the @X@ monad, this
-- approach does not quite apply to actions involving them. However a relatively
-- direct translation to impure actions is possible: you can write composable,
-- refresh-tracking actions as @X Any@ values, making sure to eschew
-- refresh-inducing functions like @windows@ and @sendMessage@ in favour of
-- 'modifyWindowSet' and utilities provided by "XMonad.Actions.MessageFeedback".
-- The 'windowBracket_' function recently added to "XMonad.Operations" is the
-- impure analogue of @defile@. Note that @PureX Any@ actions can be composed
-- into impure ones after applying 'toX'; don't use @defile@ for this. E.g.
--
-- > windowBracket_ (composableImpureAction <> toX composablePureAction)
--
-- Although both @X@ and @PureX@ have Monoid instances over monoidal values,
-- @(XLike m, Monoid a)@ is not enough to infer @Monoid (m a)@ (due to the
-- open-world assumption). Hence a @Monoid (m Any)@ constraint may need to be
-- used when working with @XLike m => m Any@ where no context is forcing @m@ to
-- unify with @X@ or @PureX@. This can also be avoided by working with
-- @PureX Any@ values and generalising them with 'toXLike' where necessary.
--
-- @PureX@ also enables a more monadic style when writing windowset operations;
-- see the implementation of the utilities in this module for examples.
-- For an example of a whole module written in terms of this one, see
-- "XMonad.Hooks.RefocusLast".
--

-- }}}

-- --< Core >-- {{{

-- | The @PureX@ newtype over @ReaderT XConf (State XState) a@.
newtype PureX a = PureX (ReaderT XConf (State XState) a)
  deriving (Functor, Applicative, Monad, MonadReader XConf, MonadState XState)

instance Semigroup a => Semigroup (PureX a) where
  (<>) = liftA2 (<>)

instance Monoid a => Monoid (PureX a) where
  mappend = liftA2 mappend
  mempty  = return mempty

-- | The @XLike@ typeclass over monads reading @XConf@ values and tracking
--   @XState@ state.
class (MonadReader XConf m, MonadState XState m) => XLike m where
  toX :: m a -> X a

instance XLike X where
  toX = id

instance XLike PureX where
  toX = toXLike

-- | Consume a @PureX a@.
runPureX :: PureX a -> XConf -> XState -> (a, XState)
runPureX (PureX m) = runState . runReaderT m

-- | Despite appearing less general, @PureX a@ is actually isomorphic to
--   @XLike m => m a@.
toXLike :: XLike m => PureX a -> m a
toXLike pa = state =<< runPureX pa <$> ask

-- | A generalisation of 'windowBracket'. Handles refreshing for an action that
--   __performs no refresh of its own__ but can indicate that it needs one
--   through a return value that's tested against the supplied predicate. The
--   action can interleave changes to the @WindowSet@ with @IO@ or changes to
--   the @XState@.
windowBracket' :: XLike m => (a -> Bool) -> m a -> X a
windowBracket' p = windowBracket p . toX

-- | A version of @windowBracket'@ specialised to take a @PureX Any@ action and
--   handle windowset changes with a refresh when the @Any@ holds @True@.
--   Analogous to 'windowBracket_'. Don't bake this into your action; it's for
--   the end-user.
defile :: PureX Any -> X ()
defile = void . windowBracket' getAny

-- | A version of @windowBracket@ specialised to take an @X ()@ action and
--   perform a refresh handling any changes it makes.
handlingRefresh :: X () -> X ()
handlingRefresh = windowBracket (\_ -> True)

-- }}}

-- --< Utility >-- {{{

-- | A 'when' that accepts a monoidal return value.
when' :: (Monad m, Monoid a) => Bool -> m a -> m a
when' b ma = if b then ma else return mempty

-- | A @whenX@/@whenM@ that accepts a monoidal return value.
whenM' :: (Monad m, Monoid a) => m Bool -> m a -> m a
whenM' mb m = when' <$> mb >>= ($ m)

-- | A 'whenJust' that accepts a monoidal return value.
whenJust' :: (Monad m, Monoid b) => Maybe a -> (a -> m b) -> m b
whenJust' = flip $ maybe (return mempty)

-- | Akin to @<*@. Discarding the wrapped value in the second argument either
--   way, keep its effects iff the first argument returns @Any True@.
(<?) :: Monad m => m Any -> m a -> m Any
ifthis <? thenthis = do
  Any b <- ifthis
  when' b (Any b <$ thenthis)
infixl 4 <?

-- | Akin to a low precedence @<>@. Combines applicative effects left-to-right
--   and wrapped @Bool@s with @&&@ (instead of @||@).
(&>) :: Applicative f => f Any -> f Any -> f Any
(&>) = liftA2 $ \(Any b1) (Any b2) -> Any (b1 && b2)
infixl 1 &>

-- | A generalisation of 'withWindowSet'.
withWindowSet' :: XLike m => (WindowSet -> m a) -> m a
withWindowSet' = (=<< gets windowset)

-- | If there is a current tag and a focused window, perform an operation with
--   them, otherwise return mempty.
withFocii :: (XLike m, Monoid a) => (WorkspaceId -> Window -> m a) -> m a
withFocii f = join $ (whenJust' <$> peek) <*> (f <$> curTag)

-- | A generalisation of 'modifyWindowSet'.
modifyWindowSet' :: XLike m => (WindowSet -> WindowSet) -> m ()
modifyWindowSet' f = modify $ \xs -> xs { windowset = f (windowset xs) }

-- | A variant of @W.modify@ and @W.modify'@ handling the @Nothing@ and @Just@
--   cases uniformly.
modify''
  :: (Maybe (W.Stack a) -> Maybe (W.Stack a))
  -> (W.StackSet i l a s sd -> W.StackSet i l a s sd)
modify'' f = W.modify (f Nothing) (f . Just)

-- | Get the stack from the current workspace.
getStack :: XLike m => m (Maybe (W.Stack Window))
getStack = W.stack <$> curWorkspace

-- | Set the stack on the current workspace.
putStack :: XLike m => Maybe (W.Stack Window) -> m ()
putStack mst = modifyWindowSet' . modify'' $ \_ -> mst

-- | Get the focused window if there is one.
peek :: XLike m => m (Maybe Window)
peek = withWindowSet' (return . W.peek)

-- | Get the current screen.
curScreen
  :: XLike m
  => m (W.Screen WorkspaceId (Layout Window) Window ScreenId ScreenDetail)
curScreen = withWindowSet' (return . W.current)

-- | Get the current workspace.
curWorkspace :: XLike m => m WindowSpace
curWorkspace = W.workspace <$> curScreen

-- | Get the current tag.
curTag :: XLike m => m WorkspaceId
curTag = W.tag <$> curWorkspace

-- | Get the current @ScreenId@.
curScreenId :: XLike m => m ScreenId
curScreenId = W.screen <$> curScreen

-- | Internal. Refresh-tracking logic of view operations.
viewWith
  :: XLike m => (WorkspaceId -> WindowSet -> WindowSet) -> WorkspaceId -> m Any
viewWith viewer tag = do
  itag <- curTag
  when' (tag /= itag) $ do
    modifyWindowSet' (viewer tag)
    Any . (tag ==) <$> curTag

-- | A version of @W.view@ that tracks the need to refresh.
view :: XLike m => WorkspaceId -> m Any
view = viewWith W.view

-- | A version of @W.greedyView@ that tracks the need to refresh.
greedyView :: XLike m => WorkspaceId -> m Any
greedyView = viewWith W.greedyView

-- | View a workspace if it's not visible. An alternative to @view@ and
--   @greedyView@ that—rather than changing the current screen or affecting
--   another—opts not to act.
invisiView :: XLike m => WorkspaceId -> m Any
invisiView = viewWith $ \tag ws ->
  if   tag `elem` (W.tag . W.workspace <$> W.current ws : W.visible ws)
  then W.view tag ws
  else ws

-- | A refresh-tracking version of @W.Shift@.
shift :: XLike m => WorkspaceId -> m Any
shift tag = withFocii $ \ctag fw ->
  when' (tag /= ctag) $ do
    modifyWindowSet' (W.shiftWin tag fw)
    mfw' <- peek
    return (Any $ Just fw /= mfw')

-- | Internal. Refresh-tracking logic of focus operations.
focusWith :: XLike m => (WindowSet -> WindowSet) -> m Any
focusWith focuser = do
    old <- peek
    modifyWindowSet' focuser
    new <- peek
    return (Any $ old /= new)

-- | A refresh-tracking version of @W.focusWindow@.
focusWindow :: XLike m => Window -> m Any
focusWindow w = focusWith (W.focusWindow w)

-- | A refresh-tracking version of @XMonad.Actions.FocusNth.focusNth@.
focusNth :: XLike m => Int -> m Any
focusNth i = focusWith (W.modify' (XMonad.Actions.FocusNth.focusNth' i))

-- }}}

