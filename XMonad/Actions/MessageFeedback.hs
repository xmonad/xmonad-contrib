-----------------------------------------------------------------------------
-- |
-- Module       : XMonad.Actions.MessageFeedback
-- Copyright    : (c) Quentin Moser <quentin.moser@unifr.ch>
-- License      : BSD3
--
-- Maintainer   : None
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
                                      , tryMessage
                                      , tryMessage_
                                      , tryInOrder
                                      , tryInOrder_
                                      , sm
                                      , sendSM
                                      , sendSM_
                                      ) where

import XMonad.Core ( X (), Message, SomeMessage(..), LayoutClass(..), windowset, catchX )
import XMonad.StackSet ( current, workspace, layout, tag )
import XMonad.Operations ( updateLayout )

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

-- | Sends the first message, and if it was not handled, sends the second.
-- Returns True if either message was handled, False otherwise.
tryMessage :: (Message a, Message b) => a -> b -> X Bool
tryMessage m1 m2 = do b <- send m1
                      if b then return True else send m2

tryMessage_ :: (Message a, Message b) => a -> b -> X ()
tryMessage_ m1 m2 = tryMessage m1 m2 >> return ()

-- | Tries sending every message of the list in order until one of them
-- is handled. Returns True if one of the messages was handled, False otherwise.
tryInOrder :: [SomeMessage] -> X Bool
tryInOrder [] = return False
tryInOrder (m:ms) = do b <- sendSM m
                       if b then return True else tryInOrder ms

tryInOrder_ :: [SomeMessage] -> X ()
tryInOrder_ ms = tryInOrder ms >> return ()


-- | Convenience shorthand for 'XMonad.Core.SomeMessage'.
sm :: Message a => a -> SomeMessage
sm = SomeMessage


sendSM :: SomeMessage -> X Bool
sendSM m = do w <- workspace . current <$> gets windowset
              ml' <- handleMessage (layout w) m `catchX` return Nothing
              updateLayout (tag w) ml'
              return $ isJust ml'


sendSM_ :: SomeMessage -> X ()
sendSM_ m = sendSM m >> return ()