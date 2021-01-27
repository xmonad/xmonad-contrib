{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses, PatternGuards #-}

-----------------------------------------------------------------------------
-- |
-- Module       : XMonad.Layout.LayoutModifier
-- Copyright    : (c) David Roundy <droundy@darcs.net>
-- License      : BSD
--
-- Maintainer   : none
-- Stability    : unstable
-- Portability  : portable
--
-- A module for writing easy layout modifiers, which do not define a
-- layout in and of themselves, but modify the behavior of or add new
-- functionality to other layouts.  If you ever find yourself writing
-- a layout which takes another layout as a parameter, chances are you
-- should be writing a LayoutModifier instead!
--
-- In case it is not clear, this module is not intended to help you
-- configure xmonad, it is to help you write other extension modules.
-- So get hacking!
-----------------------------------------------------------------------------

module XMonad.Layout.LayoutModifier (
    -- * Usage
    -- $usage

    -- * The 'LayoutModifier' class
    LayoutModifier(..), ModifiedLayout(..)
    ) where

import Control.Monad

import XMonad
import XMonad.StackSet ( Stack, Workspace (..) )

-- $usage
--
-- The 'LayoutModifier' class is provided to help extension developers
-- write easy layout modifiers.  End users won't find much of interest
-- here. =)
--
-- To write a layout modifier using the 'LayoutModifier' class, define
-- a data type to represent the layout modification (storing any
-- necessary state), define an instance of 'LayoutModifier', and
-- export an appropriate function for applying the modifier.  For example:
--
-- > data MyModifier a = MyModifier MyState
-- >   deriving (Show, Read)
-- >
-- > instance LayoutModifier MyModifier a where
-- >   -- override whatever methods from LayoutModifier you like
-- >
-- > modify :: l a -> ModifiedLayout MyModifier l a
-- > modify = ModifiedLayout (MyModifier initialState)
--
-- When defining an instance of 'LayoutModifier', you are free to
-- override as many or as few of the methods as you see fit.  See the
-- documentation below for specific information about the effect of
-- overriding each method.  Every method has a default implementation;
-- an instance of 'LayoutModifier' which did not provide a non-default
-- implementation of any of the methods would simply act as the
-- identity on any layouts to which it is applied.
--
-- For more specific usage examples, see
--
-- * "XMonad.Layout.WorkspaceDir"
--
-- * "XMonad.Layout.Magnifier"
--
-- * "XMonad.Layout.NoBorders"
--
-- * "XMonad.Layout.Reflect"
--
-- * "XMonad.Layout.Named"
--
-- * "XMonad.Layout.WindowNavigation"
--
-- and several others.  You probably want to start by looking at some
-- of the above examples; the documentation below is detailed but
-- possibly confusing, and in many cases the creation of a
-- 'LayoutModifier' is actually quite simple.
--
-- /Important note/: because of the way the 'LayoutModifier' class is
-- intended to be used, by overriding any of its methods and keeping
-- default implementations for all the others, 'LayoutModifier'
-- methods should never be called explicitly.  It is likely that such
-- explicit calls will not have the intended effect.  Rather, the
-- 'LayoutModifier' methods should only be called indirectly through
-- the 'LayoutClass' instance for 'ModifiedLayout', since it is this
-- instance that defines the semantics of overriding the various
-- 'LayoutModifier' methods.

class (Show (m a), Read (m a)) => LayoutModifier m a where

    -- | 'modifyLayout' allows you to intercept a call to 'runLayout'
    --   /before/ it is called on the underlying layout, in order to
    --   perform some effect in the X monad, and\/or modify some of
    --   the parameters before passing them on to the 'runLayout'
    --   method of the underlying layout.
    --
    --   The default implementation of 'modifyLayout' simply calls
    --   'runLayout' on the underlying layout.
    modifyLayout :: (LayoutClass l a) =>
                    m a                             -- ^ the layout modifier
                 -> Workspace WorkspaceId (l a) a   -- ^ current workspace
                 -> Rectangle                       -- ^ screen rectangle
                 -> X ([(a, Rectangle)], Maybe (l a))
    modifyLayout _ w r = runLayout w r

    -- | Similar to 'modifyLayout', but this function also allows you
    -- update the state of your layout modifier(the second value in the
    -- outer tuple).
    --
    -- If both 'modifyLayoutWithUpdate' and 'redoLayout' return a
    -- modified state of the layout modifier, 'redoLayout' takes
    -- precedence. If this function returns a modified state, this
    -- state will internally be used in the subsequent call to
    -- 'redoLayout' as well.
    modifyLayoutWithUpdate :: (LayoutClass l a) =>
                              m a
                           -> Workspace WorkspaceId (l a) a
                           -> Rectangle
                           -> X (([(a,Rectangle)], Maybe (l a)), Maybe (m a))
    modifyLayoutWithUpdate m w r = flip (,) Nothing <$> modifyLayout m w r

    -- | 'handleMess' allows you to spy on messages to the underlying
    --   layout, in order to have an effect in the X monad, or alter
    --   the layout modifier state in some way (by returning @Just
    --   nm@, where @nm@ is a new modifier).  In all cases, the
    --   underlying layout will also receive the message as usual,
    --   after the message has been processed by 'handleMess'.
    --
    --   If you wish to possibly modify a message before it reaches
    --   the underlying layout, you should use
    --   'handleMessOrMaybeModifyIt' instead.  If you do not need to
    --   modify messages or have access to the X monad, you should use
    --   'pureMess' instead.
    --
    --   The default implementation of 'handleMess' calls 'unhook'
    --   when receiving a 'Hide' or 'ReleaseResources' method (after
    --   which it returns @Nothing@), and otherwise passes the message
    --   on to 'pureMess'.
    handleMess :: m a -> SomeMessage -> X (Maybe (m a))
    handleMess m mess | Just Hide <- fromMessage mess             = doUnhook
                      | Just ReleaseResources <- fromMessage mess = doUnhook
                      | otherwise = return $ pureMess m mess
     where doUnhook = do unhook m; return Nothing

    -- | 'handleMessOrMaybeModifyIt' allows you to intercept messages
    --   sent to the underlying layout, in order to have an effect in
    --   the X monad, alter the layout modifier state, or produce a
    --   modified message to be passed on to the underlying layout.
    --
    --   The default implementation of 'handleMessOrMaybeModifyIt'
    --   simply passes on the message to 'handleMess'.
    handleMessOrMaybeModifyIt :: m a -> SomeMessage -> X (Maybe (Either (m a) SomeMessage))
    handleMessOrMaybeModifyIt m mess = do mm' <- handleMess m mess
                                          return (Left <$> mm')

    -- | 'pureMess' allows you to spy on messages sent to the
    --   underlying layout, in order to possibly change the layout
    --   modifier state.
    --
    --   The default implementation of 'pureMess' ignores messages
    --   sent to it, and returns @Nothing@ (causing the layout
    --   modifier to remain unchanged).
    pureMess :: m a -> SomeMessage -> Maybe (m a)
    pureMess _ _ = Nothing

    -- | 'redoLayout' allows you to intercept a call to 'runLayout' on
    --   workspaces with at least one window, /after/ it is called on
    --   the underlying layout, in order to perform some effect in the
    --   X monad, possibly return a new layout modifier, and\/or
    --   modify the results of 'runLayout' before returning them.
    --
    --   If you don't need access to the X monad, use 'pureModifier'
    --   instead.  Also, if the behavior you need can be cleanly
    --   separated into an effect in the X monad, followed by a pure
    --   transformation of the results of 'runLayout', you should
    --   consider implementing 'hook' and 'pureModifier' instead of
    --   'redoLayout'.
    --
    --   On empty workspaces, the Stack is Nothing.
    --
    --   The default implementation of 'redoLayout' calls 'hook' and
    --   then 'pureModifier'.
    redoLayout :: m a               -- ^ the layout modifier
               -> Rectangle         -- ^ screen rectangle
               -> Maybe (Stack a)   -- ^ current window stack
               -> [(a, Rectangle)]  -- ^ (window,rectangle) pairs returned
                                    -- by the underlying layout
               -> X ([(a, Rectangle)], Maybe (m a))
    redoLayout m r ms wrs = do hook m; return $ pureModifier m r ms wrs

    -- | 'pureModifier' allows you to intercept a call to 'runLayout'
    --   /after/ it is called on the underlying layout, in order to
    --   modify the list of window\/rectangle pairings it has returned,
    --   and\/or return a new layout modifier.
    --
    --   The default implementation of 'pureModifier' returns the
    --   window rectangles unmodified.
    pureModifier :: m a               -- ^ the layout modifier
                 -> Rectangle         -- ^ screen rectangle
                 -> Maybe (Stack a)   -- ^ current window stack
                 -> [(a, Rectangle)]  -- ^ (window, rectangle) pairs returned
                                      -- by the underlying layout
                 -> ([(a, Rectangle)], Maybe (m a))
    pureModifier _ _ _ wrs = (wrs, Nothing)

    -- | 'hook' is called by the default implementation of
    --   'redoLayout', and as such represents an X action which is to
    --   be run each time 'runLayout' is called on the underlying
    --   layout, /after/ 'runLayout' has completed.  Of course, if you
    --   override 'redoLayout', then 'hook' will not be called unless
    --   you explicitly call it.
    --
    --   The default implementation of 'hook' is @return ()@ (i.e., it
    --   has no effect).
    hook :: m a -> X ()
    hook _ = return ()

    -- | 'unhook' is called by the default implementation of
    --   'handleMess' upon receiving a 'Hide' or a 'ReleaseResources'
    --   message.
    --
    --   The default implementation, of course, does nothing.
    unhook :: m a -> X ()
    unhook _ = return ()

    -- | 'modifierDescription' is used to give a String description to
    --   this layout modifier.  It is the empty string by default; you
    --   should only override this if it is important that the
    --   presence of the layout modifier be displayed in text
    --   representations of the layout (for example, in the status bar
    --   of a "XMonad.Hooks.DynamicLog" user).
    modifierDescription :: m a -> String
    modifierDescription = const ""

    -- | 'modifyDescription' gives a String description for the entire
    --   layout (modifier + underlying layout).  By default, it is
    --   derived from the concatenation of the 'modifierDescription'
    --   with the 'description' of the underlying layout, with a
    --   \"smart space\" in between (the space is not included if the
    --   'modifierDescription' is empty).
    modifyDescription :: (LayoutClass l a) => m a -> l a -> String
    modifyDescription m l = modifierDescription m <> description l
        where "" <> x = x
              x <> y = x ++ " " ++ y

-- | The 'LayoutClass' instance for a 'ModifiedLayout' defines the
--   semantics of a 'LayoutModifier' applied to an underlying layout.
instance (LayoutModifier m a, LayoutClass l a, Typeable m) => LayoutClass (ModifiedLayout m l) a where
    runLayout (Workspace i (ModifiedLayout m l) ms) r =
        do ((ws, ml'),mm')  <- modifyLayoutWithUpdate m (Workspace i l ms) r
           (ws', mm'') <- redoLayout (maybe m id mm') r ms ws
           let ml'' = case mm'' `mplus` mm' of
                        Just m' -> Just $ (ModifiedLayout m') $ maybe l id ml'
                        Nothing -> ModifiedLayout m <$> ml'
           return (ws', ml'')

    handleMessage (ModifiedLayout m l) mess =
        do mm' <- handleMessOrMaybeModifyIt m mess
           ml' <- case mm' of
                  Just (Right mess') -> handleMessage l mess'
                  _ -> handleMessage l mess
           return $ case mm' of
                    Just (Left m') -> Just $ (ModifiedLayout m') $ maybe l id ml'
                    _ -> (ModifiedLayout m) <$> ml'
    description (ModifiedLayout m l) = modifyDescription m l

-- | A 'ModifiedLayout' is simply a container for a layout modifier
--   combined with an underlying layout.  It is, of course, itself a
--   layout (i.e. an instance of 'LayoutClass').
data ModifiedLayout m l a = ModifiedLayout (m a) (l a) deriving ( Read, Show )

-- N.B. I think there is a Haddock bug here; the Haddock output for
-- the above does not parenthesize (m a) and (l a), which is obviously
-- incorrect.

