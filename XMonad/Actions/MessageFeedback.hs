-----------------------------------------------------------------------------
-- |
-- Module       : XMonad.Actions.MessageFeedback
-- Description :  An alternative @sendMessage@.
-- Copyright    : (c) --   Quentin Moser <moserq@gmail.com>
--                    2018 Yclept Nemo
-- License      : BSD3
--
-- Maintainer   : orphaned
-- Stability    : unstable
-- Portability  : unportable
--
-- Alternative to 'XMonad.Operations.sendMessage' that provides knowledge
-- of whether the message was handled, and utility functions based on
-- this facility.
-----------------------------------------------------------------------------

module XMonad.Actions.MessageFeedback
    ( -- * Usage
      -- $usage

      -- * Messaging variants

      -- ** 'SomeMessage'
      sendSomeMessageB, sendSomeMessage
    , sendSomeMessageWithNoRefreshB, sendSomeMessageWithNoRefresh
    , sendSomeMessageWithNoRefreshToCurrentB, sendSomeMessageWithNoRefreshToCurrent

      -- ** 'Message'
    , sendMessageB
    , sendMessageWithNoRefreshB
    , sendMessageWithNoRefreshToCurrentB, sendMessageWithNoRefreshToCurrent

      -- * Utility Functions

      -- ** Send All
    , sendSomeMessagesB, sendSomeMessages, sendMessagesB, sendMessages

      -- ** Send Until
    , tryInOrderB, tryInOrderWithNoRefreshToCurrentB, tryInOrderWithNoRefreshToCurrent
    , tryMessageB, tryMessageWithNoRefreshToCurrentB, tryMessageWithNoRefreshToCurrent

      -- ** Aliases
    , sm

      -- * Backwards Compatibility
      -- $backwardsCompatibility
    , send, sendSM, sendSM_
    , tryInOrder, tryInOrder_
    , tryMessage, tryMessage_
    ) where

import XMonad               ( Window )
import XMonad.Core          ( X(), Message, SomeMessage(..), LayoutClass(..), windowset, catchX, WorkspaceId, Layout, whenJust )
import XMonad.Operations    ( updateLayout, windowBracket, modifyWindowSet )
import XMonad.Prelude       ( isJust, liftA2, void )
import XMonad.StackSet      ( Workspace, current, workspace, layout, tag )

import Control.Monad.State  ( gets )

-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Actions.MessageFeedback
--
-- You can then use this module's functions wherever an action is expected. All
-- feedback variants are supported:
--
-- * message to any workspace with no refresh
-- * message to current workspace with no refresh
-- * message to current workspace with refresh
--
-- Except "message to any workspace with refresh" which makes little sense.
--
-- Note that most functions in this module have a return type of @X Bool@
-- whereas configuration options will expect a @X ()@ action. For example, the
-- key binding:
--
-- > -- Shrink the master area of a tiled layout, or move the focused window
-- > -- to the left in a WindowArranger-based layout
-- > ((modKey, xK_Left), tryMessageWithNoRefreshToCurrentB Shrink (MoveLeft 50))
--
-- is mis-typed. For this reason, this module provides alternatives (not ending
-- with an uppercase \"B\", e.g. 'XMonad.Operations.sendMessage' rather than
-- 'sendMessageB') that discard their boolean result and return an @X ()@. For
-- example, to correct the previous example:
--
-- > ((modKey, xK_Left), tryMessageWithNoRefreshToCurrent Shrink (MoveLeft 50))
--
-- This module also provides 'SomeMessage' variants of each 'Message' function
-- for when the messages are of differing types (but still instances of
-- 'Message'). First box each message using 'SomeMessage' or the convenience
-- alias 'sm'. Then, for example, to send each message:
--
-- > sendSomeMessages [sm messageOfTypeA, sm messageOfTypeB]
--
-- This is /not/ equivalent to the following example, which will not refresh
-- the workspace unless the last message is handled:
--
-- > sendMessageWithNoRefreshToCurrent messageOfTypeA >> sendMessage messageOfTypeB


-- | Variant of 'XMonad.Operations.sendMessage'. Accepts 'SomeMessage'; to use
-- 'Message' see 'sendMessageB'. Returns @True@ if the message was handled,
-- @False@ otherwise. Instead of using 'sendSomeMessageWithNoRefreshToCurrentB'
-- for efficiency this is pretty much an exact copy of the
-- 'XMonad.Operations.sendMessage' code - foregoes the O(n) 'updateLayout'.
sendSomeMessageB :: SomeMessage -> X Bool
sendSomeMessageB m = windowBracket id $ do
    w  <- gets ((workspace . current) . windowset)
    ml <- handleMessage (layout w) m `catchX` return Nothing
    whenJust ml $ \l ->
        modifyWindowSet $ \ws -> ws { current = (current ws)
                                { workspace = (workspace $ current ws)
                                    { layout = l }}}
    return $ isJust ml

-- | Variant of 'sendSomeMessageB' that discards the result.
sendSomeMessage :: SomeMessage -> X ()
sendSomeMessage = void . sendSomeMessageB

-- | Variant of 'XMonad.Operations.sendMessageWithNoRefresh'. Accepts
-- 'SomeMessage'; to use 'Message' see 'sendMessageWithNoRefreshB'. Returns
-- @True@ if the message was handled, @False@ otherwise.
sendSomeMessageWithNoRefreshB :: SomeMessage -> Workspace WorkspaceId (Layout Window) Window -> X Bool
sendSomeMessageWithNoRefreshB m w
    =   handleMessage (layout w) m `catchX` return Nothing
    >>= liftA2 (>>) (updateLayout $ tag w) (return . isJust)

-- | Variant of 'sendSomeMessageWithNoRefreshB' that discards the result.
sendSomeMessageWithNoRefresh :: SomeMessage -> Workspace WorkspaceId (Layout Window) Window -> X ()
sendSomeMessageWithNoRefresh m = void . sendSomeMessageWithNoRefreshB m

-- | Variant of 'XMonad.Operations.sendMessageWithNoRefresh' that sends the
-- message to the current layout. Accepts 'SomeMessage'; to use 'Message' see
-- 'sendMessageWithNoRefreshToCurrentB'. Returns @True@ if the message was
-- handled, @False@ otherwise. This function is somewhat of a cross between
-- 'XMonad.Operations.sendMessage' (sends to the current layout) and
-- 'XMonad.Operations.sendMessageWithNoRefresh' (does not refresh).
sendSomeMessageWithNoRefreshToCurrentB :: SomeMessage -> X Bool
sendSomeMessageWithNoRefreshToCurrentB m
    =   gets (workspace . current . windowset)
    >>= sendSomeMessageWithNoRefreshB m

-- | Variant of 'sendSomeMessageWithNoRefreshToCurrentB' that discards the
-- result.
sendSomeMessageWithNoRefreshToCurrent :: SomeMessage -> X ()
sendSomeMessageWithNoRefreshToCurrent = void . sendSomeMessageWithNoRefreshToCurrentB


-- | Variant of 'sendSomeMessageB' which like 'XMonad.Operations.sendMessage'
-- accepts 'Message' rather than 'SomeMessage'. Returns @True@ if the message
-- was handled, @False@ otherwise.
sendMessageB :: Message a => a -> X Bool
sendMessageB = sendSomeMessageB . SomeMessage

-- | Variant of 'sendSomeMessageWithNoRefreshB' which like
-- 'XMonad.Operations.sendMessageWithNoRefresh' accepts 'Message' rather than
-- 'SomeMessage'. Returns @True@ if the message was handled, @False@ otherwise.
sendMessageWithNoRefreshB :: Message a => a -> Workspace WorkspaceId (Layout Window) Window -> X Bool
sendMessageWithNoRefreshB = sendSomeMessageWithNoRefreshB . SomeMessage

-- | Variant of 'sendSomeMessageWithNoRefreshToCurrentB' which accepts
-- 'Message' rather than 'SomeMessage'. Returns @True@ if the message was
-- handled, @False@ otherwise.
sendMessageWithNoRefreshToCurrentB :: Message a => a -> X Bool
sendMessageWithNoRefreshToCurrentB = sendSomeMessageWithNoRefreshToCurrentB . SomeMessage

-- | Variant of 'sendMessageWithNoRefreshToCurrentB' that discards the result.
sendMessageWithNoRefreshToCurrent :: Message a => a -> X ()
sendMessageWithNoRefreshToCurrent = void . sendMessageWithNoRefreshToCurrentB


-- | Send each 'SomeMessage' to the current layout without refresh (using
-- 'sendSomeMessageWithNoRefreshToCurrentB') and collect the results. If any
-- message was handled, refresh. If you want to sequence a series of messages
-- that would have otherwise used 'XMonad.Operations.sendMessage' while
-- minimizing refreshes, use this.
sendSomeMessagesB :: [SomeMessage] -> X [Bool]
sendSomeMessagesB
    = windowBracket or
    . mapM sendSomeMessageWithNoRefreshToCurrentB

-- | Variant of 'sendSomeMessagesB' that discards the results.
sendSomeMessages :: [SomeMessage] -> X ()
sendSomeMessages = void . sendSomeMessagesB

-- | Variant of 'sendSomeMessagesB' which accepts 'Message' rather than
-- 'SomeMessage'. Use this if all the messages are of the same type.
sendMessagesB :: Message a => [a] -> X [Bool]
sendMessagesB = sendSomeMessagesB . map SomeMessage

-- | Variant of 'sendMessagesB' that discards the results.
sendMessages :: Message a => [a] -> X ()
sendMessages = void . sendMessagesB


-- | Apply the dispatch function in order to each message of the list until one
-- is handled. Returns @True@ if so, @False@ otherwise.
tryInOrderB :: (SomeMessage -> X Bool) -> [SomeMessage] -> X Bool
tryInOrderB _ []     = return False
tryInOrderB f (m:ms) = do b <- f m
                          if b then return True else tryInOrderB f ms

-- | Variant of 'tryInOrderB' that sends messages to the current layout without
-- refresh using 'sendSomeMessageWithNoRefreshToCurrentB'.
tryInOrderWithNoRefreshToCurrentB :: [SomeMessage] -> X Bool
tryInOrderWithNoRefreshToCurrentB = tryInOrderB sendSomeMessageWithNoRefreshToCurrentB

-- | Variant of 'tryInOrderWithNoRefreshToCurrent' that discards the results.
tryInOrderWithNoRefreshToCurrent :: [SomeMessage] -> X ()
tryInOrderWithNoRefreshToCurrent = void . tryInOrderWithNoRefreshToCurrentB

-- | Apply the dispatch function to the first message, and if it was not
-- handled, apply it to the second. Returns @True@ if either message was
-- handled, @False@ otherwise.
tryMessageB :: (Message a, Message b) => (SomeMessage -> X Bool) -> a -> b -> X Bool
tryMessageB f m1 m2 = tryInOrderB f [sm m1,sm m2]

-- | Variant of 'tryMessageB' that sends messages to the current layout without
-- refresh using 'sendMessageWithNoRefreshToCurrentB'.
tryMessageWithNoRefreshToCurrentB :: (Message a, Message b) => a -> b -> X Bool
tryMessageWithNoRefreshToCurrentB = tryMessageB sendSomeMessageWithNoRefreshToCurrentB

-- | Variant of 'tryMessage' that discards the results.
tryMessageWithNoRefreshToCurrent :: (Message a, Message b) => a -> b -> X ()
tryMessageWithNoRefreshToCurrent m = void . tryMessageWithNoRefreshToCurrentB m


-- | Convenience shorthand for 'SomeMessage'.
sm :: Message a => a -> SomeMessage
sm = SomeMessage

--------------------------------------------------------------------------------
-- Backwards Compatibility:
--------------------------------------------------------------------------------
{-# DEPRECATED send "Use sendMessageB instead." #-}
{-# DEPRECATED sendSM "Use sendSomeMessageB instead." #-}
{-# DEPRECATED sendSM_ "Use sendSomeMessage instead." #-}
{-# DEPRECATED tryInOrder "Use tryInOrderWithNoRefreshToCurrentB instead." #-}
{-# DEPRECATED tryInOrder_ "Use tryInOrderWithNoRefreshToCurrent instead." #-}
{-# DEPRECATED tryMessage "Use tryMessageWithNoRefreshToCurrentB instead." #-}
{-# DEPRECATED tryMessage_ "Use tryMessageWithNoRefreshToCurrent instead." #-}

-- $backwardsCompatibility
-- The following functions exist solely for compatibility with pre-0.14
-- releases.

-- | See 'sendMessageWithNoRefreshToCurrentB'.
send :: Message a => a -> X Bool
send = sendMessageWithNoRefreshToCurrentB

-- | See 'sendSomeMessageWithNoRefreshToCurrentB'.
sendSM :: SomeMessage -> X Bool
sendSM = sendSomeMessageWithNoRefreshToCurrentB

-- | See 'sendSomeMessageWithNoRefreshToCurrent'.
sendSM_ :: SomeMessage -> X ()
sendSM_ = sendSomeMessageWithNoRefreshToCurrent

-- | See 'tryInOrderWithNoRefreshToCurrentB'.
tryInOrder :: [SomeMessage] -> X Bool
tryInOrder = tryInOrderWithNoRefreshToCurrentB

-- | See 'tryInOrderWithNoRefreshToCurrent'.
tryInOrder_ :: [SomeMessage] -> X ()
tryInOrder_ = tryInOrderWithNoRefreshToCurrent

-- | See 'tryMessageWithNoRefreshToCurrentB'.
tryMessage :: (Message a, Message b) => a -> b -> X Bool
tryMessage = tryMessageWithNoRefreshToCurrentB

-- | See 'tryMessageWithNoRefreshToCurrent'.
tryMessage_ :: (Message a, Message b) => a -> b -> X ()
tryMessage_ = tryMessageWithNoRefreshToCurrent
