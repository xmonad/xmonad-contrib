{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
--------------------------------------------------------------------
-- |
-- Module      : XMonad.Util.Parser
-- Description : A parser combinator library for xmonad
-- Copyright   : (c) 2021  Tony Zorman
-- License     : BSD3
-- Maintainer  : Tony Zorman <soliditsallgood@mailbox.org>
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
  pfail,
  eof,
  num,
  char,
  string,
  skipSpaces,
  get,
  look,
  gather,

  -- * Combining Parsers
  satisfy,
  choice,
  count,
  between,
  option,
  optionally,
  skipMany,
  skipMany1,
  many1,
  sepBy,
  sepBy1,
  endBy,
  endBy1,
  munch,
  munch1,
  chainr,
  chainr1,
  chainl,
  chainl1,
  manyTill,
) where

import XMonad.Prelude

import qualified Text.ParserCombinators.ReadP as ReadP

import Data.Coerce (coerce)
import Data.String (IsString (fromString))
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
  {-# INLINE (<>) #-}

instance Monoid (Parser a) where
  -- | A parser that always fails.
  mempty :: Parser a
  mempty = Parser empty
  {-# INLINE mempty #-}

instance Alternative Parser where
  empty :: Parser a
  empty = mempty
  {-# INLINE empty #-}

  (<|>) :: Parser a -> Parser a -> Parser a
  (<|>) = (<>)
  {-# INLINE (<|>) #-}

-- | When @-XOverloadedStrings@ is on, treat a string @s@ as the parser
-- @'string' s@, when appropriate.  This allows one to write things like
-- @"a" *> otherParser@ instead of @'string' "a" *> otherParser@.
instance a ~ String => IsString (Parser a) where
  fromString :: String -> Parser a
  fromString = string
  {-# INLINE fromString #-}

-- | Run a parser on a given string.
runParser :: Parser a -> String -> Maybe a
runParser (Parser p) = fmap fst . listToMaybe . ReadP.readP_to_S p
{-# INLINE runParser #-}

-- | Always fails
pfail :: Parser a
pfail = empty
{-# INLINE pfail #-}

-- | Consume and return the next character.  Fails if there is no input
-- left.
get :: Parser Char
get = coerce ReadP.get
{-# INLINE get #-}

-- | Look-ahead: return the part of the input that is left, without
-- consuming it.
look :: Parser String
look = coerce ReadP.look
{-# INLINE look #-}

-- | Transform a parser into one that does the same, but in addition
-- returns the exact characters read.
--
-- >>> runParser (         string "* " $> True) "* hi"
-- Just True
-- >>> runParser (gather $ string "* " $> True) "* hi"
-- Just ("* ",True)
gather :: forall a. Parser a -> Parser (String, a)
gather = coerce (ReadP.gather @a)
{-# INLINE gather #-}

-- | Succeeds if and only if we are at the end of input.
eof :: Parser ()
eof = coerce ReadP.eof
{-# INLINE eof #-}

-- | Parse an integral number.
num :: (Read a, Integral a) => Parser a
num = read <$> munch1 isDigit
{-# INLINE num #-}

-- | Parse and return the specified character.
char :: Char -> Parser Char
char = coerce ReadP.char
{-# INLINE char #-}

-- | Parse and return the specified string.
string :: String -> Parser String
string = coerce ReadP.string
{-# INLINE string #-}

-- | Skip all whitespace.
skipSpaces :: Parser ()
skipSpaces = coerce ReadP.skipSpaces
{-# INLINE skipSpaces #-}

-- | Consume and return the next character if it satisfies the specified
-- predicate.
satisfy :: (Char -> Bool) -> Parser Char
satisfy = coerce ReadP.satisfy
{-# INLINE satisfy #-}

-- | Combine all parsers in the given list in a left-biased way.
choice :: [Parser a] -> Parser a
choice = foldl' (<>) mempty
{-# INLINE choice #-}

-- | @count n p@ parses @n@ occurrences of @p@ in sequence and returns a
-- list of results.
count :: Int -> Parser a -> Parser [a]
count = replicateM
{-# INLINE count #-}

-- | @between open close p@ parses @open@, followed by @p@ and finally
-- @close@.  Only the value of @p@ is returned.
between :: Parser open -> Parser close -> Parser a -> Parser a
between open close p = open *> p <* close
{-# INLINE between #-}

-- | @option def p@ will try to parse @p@ and, if it fails, simply
-- return @def@ without consuming any input.
option :: a -> Parser a -> Parser a
option def p = p <|> pure def
{-# INLINE option #-}

-- | @optionally p@ optionally parses @p@ and always returns @()@.
optionally :: Parser a -> Parser ()
optionally p = void p <|> pure ()
{-# INLINE optionally #-}

-- | Like 'many', but discard the result.
skipMany :: Parser a -> Parser ()
skipMany = void . many
{-# INLINE skipMany #-}

-- | Like 'many1', but discard the result.
skipMany1 :: Parser a -> Parser ()
skipMany1 p = p *> skipMany p
{-# INLINE skipMany1 #-}

-- | Parse the first zero or more characters satisfying the predicate.
-- Always succeeds; returns an empty string if the predicate returns
-- @False@ on the first character of input.
munch :: (Char -> Bool) -> Parser String
munch = coerce ReadP.munch
{-# INLINE munch #-}

-- | Parse the first one or more characters satisfying the predicate.
-- Fails if none, else succeeds exactly once having consumed all the
-- characters.
munch1 :: (Char -> Bool) -> Parser String
munch1 = coerce ReadP.munch1
{-# INLINE munch1 #-}

-- | @endBy p sep@ parses zero or more occurrences of @p@, separated and
-- ended by @sep@.
endBy :: Parser a -> Parser sep -> Parser [a]
endBy p sep = many (p <* sep)
{-# INLINE endBy #-}

-- | @endBy p sep@ parses one or more occurrences of @p@, separated and
-- ended by @sep@.
endBy1 :: Parser a -> Parser sep -> Parser [a]
endBy1 p sep = many1 (p <* sep)
{-# INLINE endBy1 #-}

-- | Parse one or more occurrences of the given parser.
many1 :: Parser a -> Parser [a]
many1 = some
{-# INLINE many1 #-}

-- | @sepBy p sep@ parses zero or more occurrences of @p@, separated by
-- @sep@.  Returns a list of values returned by @p@.
sepBy :: Parser a -> Parser sep -> Parser [a]
sepBy p sep = sepBy1 p sep <> pure []
{-# INLINE sepBy #-}

-- | @sepBy1 p sep@ parses one or more occurrences of @p@, separated by
-- @sep@.  Returns a list of values returned by @p@.
sepBy1 :: Parser a -> Parser sep -> Parser [a]
sepBy1 p sep = liftA2 (:) p (many (sep *> p))
{-# INLINE sepBy1 #-}

-- | @chainr p op x@ parses zero or more occurrences of @p@, separated
-- by @op@.  Returns a value produced by a /right/ associative
-- application of all functions returned by @op@.  If there are no
-- occurrences of @p@, @x@ is returned.
chainr :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainr p op x = option x (chainr1 p op)
{-# INLINE chainr #-}

-- | Like 'chainr', but parses one or more occurrences of @p@.
chainr1 :: forall a. Parser a -> Parser (a -> a -> a) -> Parser a
chainr1 p op = scan
 where
  scan :: Parser a
  scan = p >>= rest

  rest :: a -> Parser a
  rest x = option x $ do f <- op
                         f x <$> scan
{-# INLINE chainr1 #-}

-- | @chainl p op x@ parses zero or more occurrences of @p@, separated
-- by @op@.  Returns a value produced by a /left/ associative
-- application of all functions returned by @op@.  If there are no
-- occurrences of @p@, @x@ is returned.
chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl p op x = option x (chainl1 p op)
{-# INLINE chainl #-}

-- | Like 'chainl', but parses one or more occurrences of @p@.
chainl1 :: forall a. Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op = scan
 where
  scan :: Parser a
  scan = p >>= rest

  rest :: a -> Parser a
  rest x = option x $ do f <- op
                         y <- p
                         rest (f x y)
{-# INLINE chainl1 #-}

-- | @manyTill p end@ parses zero or more occurrences of @p@, until
-- @end@ succeeds.  Returns a list of values returned by @p@.
manyTill :: forall a end. Parser a -> Parser end -> Parser [a]
manyTill p end = scan
 where
  scan :: Parser [a]
  scan = end $> [] <|> liftA2 (:) p scan
{-# INLINE manyTill #-}
