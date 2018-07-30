-----------------------------------------------------------------------------
-- |
-- Module       : XMonad.Actions.MessageFeedback
-- Copyright    : (c) Quentin Moser <moserq@gmail.com>
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

module XMonad.Actions.MessageFeedback (
                                      -- * Usage
                                      -- $usage

                                        send
                                      , sendWithNoRefresh
                                      , tryMessage
                                      , tryMessage_
                                      , tryMessageWithNoRefresh
                                      , tryMessageWithNoRefresh_
                                      , tryInOrder
                                      , tryInOrder_
                                      , tryInOrderWithNoRefresh
                                      , tryInOrderWithNoRefresh_
                                      , sm
                                      , sendSM
                                      , sendSM_
                                      , sendSMWithNoRefresh
                                      , sendSMWithNoRefresh_
                                      ) where

import XMonad.Core ( X (), Message, SomeMessage(..), LayoutClass(..), windowset, catchX, whenJust )
import XMonad.StackSet ( current, workspace, layout, tag )
import XMonad.Operations ( updateLayout, windows )
import qualified XMonad.StackSet as W

import Control.Monad.State ( gets )
import Data.Maybe ( isJust )
import Control.Applicative ((<$>))

-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Actions.MessageFeedback
--
-- You can then use this module's functions wherever an action is expected.
--
-- Note that most functions in this module have a return type of @X Bool@
-- whereas configuration options will expect a @X ()@ action.
-- For example, the key binding
--
-- > -- Shrink the master area of a tiled layout, or move the focused window
-- > -- to the left in a WindowArranger-based layout
-- > ((modKey, xK_Left), tryMessage Shrink (MoveLeft 50))
--
-- is mis-typed. For this reason, this module provides alternatives (ending with
-- an underscore, e.g. tryMessage_) that discard their result and return an @X ()@.
-- For example, to correct the previous example:
--
-- > ((modKey, xK_Left), tryMessage_ Shrink (MoveLeft 50))
--


-- | Behaves like 'XMonad.Operations.sendMessage', but returns True of the
-- message was handled by the layout, False otherwise.
send :: Message a => a -> X Bool
send = sendSM . sm

-- | Behaves like 'XMonad.Operations.sendMessageWithNoRefresh', but returns True of the
-- message was handled by the layout, False otherwise.
sendWithNoRefresh :: Message a => a -> X Bool
sendWithNoRefresh = sendSMWithNoRefresh . sm

-- | Sends the first message, and if it was not handled, sends the second.
-- Returns True if either message was handled, False otherwise.
tryMessage :: (Message a, Message b) => a -> b -> X Bool
tryMessage m1 m2 = do b <- send m1
                      if b then return True else send m2

-- | Like 'tryMessage', but discards the response.
tryMessage_ :: (Message a, Message b) => a -> b -> X ()
tryMessage_ m1 m2 = tryMessage m1 m2 >> return ()

-- | Like 'tryMessage', but without a refresh at the end.
tryMessageWithNoRefresh :: (Message a, Message b) => a -> b -> X Bool
tryMessageWithNoRefresh m1 m2 = do b <- sendWithNoRefresh m1
                                   if b then return True else sendWithNoRefresh m2

-- | Like 'tryMessageWithNoRefresh', but discards the response.
tryMessageWithNoRefresh_ :: (Message a, Message b) => a -> b -> X ()
tryMessageWithNoRefresh_ m1 m2 = tryMessageWithNoRefresh m1 m2 >> return ()

-- | Tries sending every message of the list in order until one of them
-- is handled. Returns True if one of the messages was handled, False otherwise.
tryInOrder :: [SomeMessage] -> X Bool
tryInOrder [] = return False
tryInOrder (m:ms) = do b <- sendSM m
                       if b then return True else tryInOrder ms

-- | Like 'tryInOrder', but discards the response.
tryInOrder_ :: [SomeMessage] -> X ()
tryInOrder_ ms = tryInOrder ms >> return ()

-- | Like 'tryInOrder', but without a refresh at the end.
tryInOrderWithNoRefresh :: [SomeMessage] -> X Bool
tryInOrderWithNoRefresh [] = return False
tryInOrderWithNoRefresh (m:ms) = do b <- sendSM m
                                    if b then return True else tryInOrder ms

-- | Like 'tryInOrderWithNoRefresh', but discards the response.
tryInOrderWithNoRefresh_ :: [SomeMessage] -> X ()
tryInOrderWithNoRefresh_ ms = tryInOrder ms >> return ()


-- | Convenience shorthand for 'XMonad.Core.SomeMessage'.
sm :: Message a => a -> SomeMessage
sm = SomeMessage


-- | Send a 'SomeMessage' to the corrent layout and returns true if the message was handled.
sendSM :: SomeMessage -> X Bool
sendSM m = do w <- workspace . current <$> gets windowset
              ml' <- handleMessage (layout w) m `catchX` return Nothing
              whenJust ml' $ \l' ->
                  windows $ \ws -> ws { W.current = (W.current ws)
                                          { W.workspace = (W.workspace $ W.current ws)
                                            { W.layout = l' }}}
              return $ isJust ml'

-- | Like 'sendSM', but discards the response.
sendSM_ :: SomeMessage -> X ()
sendSM_ m = sendSM m >> return ()


-- | Like 'SendSM', but without a refresh at the end.
sendSMWithNoRefresh :: SomeMessage -> X Bool
sendSMWithNoRefresh m = do w <- workspace . current <$> gets windowset
                           ml' <- handleMessage (layout w) m `catchX` return Nothing
                           updateLayout (tag w) ml'
                           return $ isJust ml'

-- | Like 'sendSMWithNoRefresh', but discards the response.
sendSMWithNoRefresh_ :: SomeMessage -> X ()
sendSMWithNoRefresh_ m = sendSMWithNoRefresh m >> return ()
