{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.MessageControl
-- Description :  Message escaping and filtering facilities.
-- Copyright   :  (c) 2008 Quentin Moser
-- License     :  BSD3
--
-- Maintainer  :  orphaned
-- Stability   :  unstable
-- Portability :  unportable
--
-- Provides message \"escaping\" and filtering facilities which
-- help control complex nested layouts.
-----------------------------------------------------------------------------

module XMonad.Layout.MessageControl (
                               -- * Usage
                               -- $usage
                            Ignore()
                          , ignore
                          , UnEscape()
                          , unEscape
                          , EscapedMessage(Escape)
                          , escape
                          ) where

import XMonad.Core (Message, SomeMessage(..)
                   , fromMessage, LayoutClass(..))
import XMonad.StackSet (Workspace(..))

import XMonad.Layout.LayoutModifier (LayoutModifier(..), ModifiedLayout(..))

import Control.Arrow (second)

-- $usage
-- You can use this module by importing it into your @~\/.xmonad\/xmonad.hs@ file:
--
-- > import XMonad.Layout.MessageEscape
--
-- Then, if you use a modified layout where the modifier would intercept
-- a message, but you'd want to be able to send it to the inner layout
-- only, add the 'unEscape' modifier to the inner layout like so:
--
-- > import XMonad.Layout.Master (mastered)
-- > import XMonad.Layout.Tabbed (simpleTabbed)
-- >
-- > myLayout = Tall ||| unEscape (mastered 0.01 0.5 $ Full ||| simpleTabbed)
--
-- you can now send a message to the inner layout with
--  @sendMessage $ escape message@, e.g.
--
-- > -- Change the inner layout
-- > ((modm .|. controlMask, xK_space), sendMessage $ escape NextLayout)
--
-- If you want unescaped messages to be handled /only/ by the enclosing
-- layout, use the 'ignore' modifier:
--
-- > myLayout = Tall ||| (ignore NextLayout $ ignore (JumpToLayout "") $
-- >                       unEscape $ mastered 0.01 0.5
-- >                         $ Full ||| simpleTabbed)
--

-- | the Ignore layout modifier. Prevents its inner layout from receiving
-- messages of a certain type.

newtype Ignore m l w = I (l w)
                    deriving (Show, Read)

instance (Message m, LayoutClass l w) => LayoutClass (Ignore m l) w where
    runLayout ws r = second (I <$>) <$> runLayout (unILayout ws) r
        where  unILayout :: Workspace i (Ignore m l w) w -> Workspace i (l w) w
               unILayout w@Workspace{ layout = (I l) } = w { layout = l }
    handleMessage l@(I l') sm
        = case fromMessageAs sm l of
            Just _ -> return Nothing
            Nothing -> (I <$>) <$> handleMessage l' sm
        where fromMessageAs :: Message m' => SomeMessage -> Ignore m' l w -> Maybe m'
              fromMessageAs a _ = fromMessage a
    description (I l) = "Ignore "++description l

-- | the UnEscape layout modifier. Listens to 'EscapedMessage's and sends
-- their nested message to the inner layout.

data UnEscape w = UE
                deriving (Show, Read)

instance LayoutModifier UnEscape a where
    handleMessOrMaybeModifyIt _ mess
        = return $ case fromMessage mess of
                     Just (Escape mess') -> Just $ Right mess'
                     Nothing -> Nothing


-- | Data type for an escaped message. Send with 'escape'.

newtype EscapedMessage = Escape SomeMessage
instance Message EscapedMessage


-- | Creates an 'EscapedMessage'.

escape :: Message m => m -> EscapedMessage
escape = Escape . SomeMessage


-- | Applies the UnEscape layout modifier to a layout.

unEscape :: LayoutClass l w => l w -> ModifiedLayout UnEscape l w
unEscape = ModifiedLayout UE


-- | Applies the Ignore layout modifier to a layout, blocking
-- all messages of the same type as the one passed as its first argument.

ignore :: (Message m, LayoutClass l w)
          => m -> l w -> Ignore m l w
ignore _ = I
