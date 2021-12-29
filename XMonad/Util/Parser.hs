{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE TypeApplications           #-}
--------------------------------------------------------------------
-- |
-- Module      : XMonad.Util.Parser
-- Description : A parser combinator library for xmonad
-- Copyright   : (c) 2021  slotThe <soliditsallgood@mailbox.org>
-- License     : BSD3
-- Maintainer  : slotThe <soliditsallgood@mailbox.org>
-- Stability   : experimental
-- Portability : non-portable
--
-- A small wrapper around the 'ReadP' parser combinator in @base@,
-- providing a more intuitive behaviour.  While it's theoretically nice
-- that 'ReadP' is actually commutative, this makes a lot of parsing
-- operations rather awkward—more often than not, one only wants the
-- argument that's parsed "first".
--
-- Due to the left-biased nature of the chosen semigroup implementation,
-- using functions like 'many' or 'optional' from "Control.Applicative"
-- now yields more consistent behaviour with other parser combinator
-- libraries.
--
--------------------------------------------------------------------
module XMonad.Util.Parser (
  -- * Usage
  -- $usage

  -- * Running
  Parser,
  runParser,

  -- * Primitive Parsers
  eof,
  num,
  char,
  string,
  skipSpaces,
  get,
  look,

  -- * Combining Parsers
  satisfy,
  choice,
  many1,
  sepBy,
  sepBy1,
  endBy,
  endBy1,
  munch,
  munch1,
  pfail
) where

import XMonad.Prelude

import qualified Text.ParserCombinators.ReadP as ReadP

import Data.Coerce (coerce)
import Text.ParserCombinators.ReadP (ReadP, (<++))

{- $usage

NOTE: This module is mostly intended for developing of other modules.
If you are a users, you probably won't find much use here—you have been
warned.

The high-level API tries to stay as close to 'ReadP' as possible.  If
you are familiar with that then no functions here should surprise you.

One notable usability difference when forcing left-biasedness is /when/
one wants to disambiguate a parse.  For normal 'ReadP' usage this
happens after the actual parsing stage by going through the list of
successful parses.  For 'Parser' it does when constructing the relevant
combinators, leading to only one successful parse.  As an example,
consider the 'ReadP'-based parser

> pLangle = ReadP.string "<"
> pLongerSequence = ReadP.char '<' *> ReadP.string "f" <* ReadP.char '>'
> pCombination = pLangle ReadP.+++ pLongerSequence

Parsing the string @"<f>"@ will return

>>> ReadP.readP_to_S pCombination "<f>"
[("<","f>"),("f","")]

One would now need to, for example, filter for the second (leftover)
string being empty and take the head of the resulting list (which may
still have more than one element).

With 'Parser', the same situation would look like the following

> pLangle' = string "<"
> pLongerSequence' = char '<' *> string "f" <* char '>'
> pCombination' = pLongerSequence' <> pLangle'

Notice how @pLangle'@ and @pLongerSequence'@ have traded places—since we
are not forcing @pLangle'@ to consume the entire string and @(<>)@ is
left-biased, @pLongerSequence'@ parses a superset of @pLangle'@!
Running @runParser pCombination'@ now yields the expected result:

>>> runParser pCombination' "<f>"
Just "f"

One might also define @pLangle'@ as @string "<" <* eof@, which would
enable a definition of @pCombination' = pLangle' <> pLongerSequence'@.

For example uses, see "XMonad.Util.EZConfig" or "XMonad.Prompt.OrgMode".
-}

-- Parser :: Type -> Type
newtype Parser a = Parser (ReadP a)
  deriving newtype (Functor, Applicative, Monad)

instance Semigroup (Parser a) where
  -- | Local, exclusive, left-biased choice: If left parser locally
  -- produces any result at all, then right parser is not used.
  (<>) :: Parser a -> Parser a -> Parser a
  (<>) = coerce ((<++) @a)

instance Monoid (Parser a) where
  -- | A parser that always fails.
  mempty :: Parser a
  mempty = Parser empty

instance Alternative Parser where
  empty :: Parser a
  empty = mempty

  (<|>) :: Parser a -> Parser a -> Parser a
  (<|>) = (<>)

-- | Run a parser on a given string.
runParser :: Parser a -> String -> Maybe a
runParser (Parser p) = fmap fst . listToMaybe . ReadP.readP_to_S p

-- | Always fails
pfail :: Parser a
pfail = empty

-- | Consume and return the next character.  Fails if there is no input
-- left.
get :: Parser Char
get = coerce ReadP.get

-- | Look-ahead: return the part of the input that is left, without
-- consuming it.
look :: Parser String
look = coerce ReadP.look

-- | Succeeds if and only if we are at the end of input.
eof :: Parser ()
eof = coerce ReadP.eof

-- | Parse an integral number number.
num :: (Read a, Integral a) => Parser a
num = read <$> munch1 isDigit
{-# SPECIALISE num :: Parser Word    #-}
{-# SPECIALISE num :: Parser Int     #-}
{-# SPECIALISE num :: Parser Integer #-}

-- | Parse and return the specified character.
char :: Char -> Parser Char
char = coerce ReadP.char

-- | Parse and return the specified string.
string :: String -> Parser String
string = coerce ReadP.string

-- | Skip all whitespace.
skipSpaces :: Parser ()
skipSpaces = coerce ReadP.skipSpaces

-- | Consume and return the next character if it satisfies the specified
-- predicate.
satisfy :: (Char -> Bool) -> Parser Char
satisfy = coerce ReadP.satisfy

-- | Combine all parsers in the given list in a left-biased way.
choice :: [Parser a] -> Parser a
choice = foldl' (<>) mempty

-- | Parse the first zero or more characters satisfying the predicate.
-- Always succeeds; returns an empty string if the predicate returns
-- @False@ on the first character of input.
munch :: (Char -> Bool) -> Parser String
munch = coerce ReadP.munch

-- | Parse the first one or more characters satisfying the predicate.
-- Fails if none, else succeeds exactly once having consumed all the
-- characters.
munch1 :: (Char -> Bool) -> Parser String
munch1 = coerce ReadP.munch1

-- | @endBy p sep@ parses zero or more occurrences of @p@, separated and
-- ended by @sep@.
endBy :: Parser a -> Parser sep -> Parser [a]
endBy p sep = many (p <* sep)

-- | @endBy p sep@ parses one or more occurrences of @p@, separated and
-- ended by @sep@.
endBy1 :: Parser a -> Parser sep -> Parser [a]
endBy1 p sep = many1 (p <* sep)

-- | Parse one or more occurrences of the given parser.
many1 :: Parser a -> Parser [a]
many1 p = liftA2 (:) p (many p)

-- | @sepBy p sep@ parses zero or more occurrences of @p@, separated by
-- @sep@.  Returns a list of values returned by @p@.
sepBy :: Parser a -> Parser sep -> Parser [a]
sepBy p sep = sepBy1 p sep <> pure []

-- | @sepBy1 p sep@ parses one or more occurrences of @p@, separated by
-- @sep@.  Returns a list of values returned by @p@.
sepBy1 :: Parser a -> Parser sep -> Parser [a]
sepBy1 p sep = liftA2 (:) p (many (sep *> p))
