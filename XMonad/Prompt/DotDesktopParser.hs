{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
module XMonad.Prompt.DotDesktopParser
    ( runDotDesktopParser
    ) where

import Data.Maybe ( catMaybes )
import Control.Monad ( MonadPlus(..) )
import Control.Applicative ( Alternative(..) )
import qualified Data.Map as MAP

newtype Parser a = Parser { parse :: String -> [(a,String)] }

runParser :: Parser a -> String -> Either String a
runParser m s =
  case parse m s of
    [(res, [])] -> Right res
    [(_, b)]    -> Left $ "Parser did not consume entire stream.  Remaining: " ++ show b -- ++ " " ++ show b
    _           -> Left "Parser error."

item :: Parser Char
item = Parser $ \case
   []     -> []
   (c:cs) -> [(c,cs)]

instance Functor Parser where
  fmap f (Parser cs) = Parser (\s -> [(f a, b) | (a, b) <- cs s])

instance Applicative Parser where
  pure a = Parser (\s -> [(a,s)])
  (Parser cs1) <*> (Parser cs2) = Parser (\s -> [(f a, s2) | (f, s1) <- cs1 s, (a, s2) <- cs2 s1])

instance Monad Parser where
  p >>= f = Parser $ \s -> concatMap (\(a, s') -> parse (f a) s') $ parse p s

instance MonadPlus Parser where
  mzero = failure
  mplus = combine

instance Alternative Parser where
  empty = mzero
  (<|>) = option

combine :: Parser a -> Parser a -> Parser a
combine p q = Parser (\s -> parse p s ++ parse q s)

failure :: Parser a
failure = Parser (const [])

option :: Parser a -> Parser a -> Parser a
option  p q = Parser $ \s ->
  case parse p s of
    []     -> parse q s
    res    -> res

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = item >>= \c ->
  if p c
  then return c
  else failure

type Predicate a = a -> Bool

notP :: Predicate a -> Predicate a
notP = (not .)

-------------------------------------------------------------------------------
-- Combinators
-------------------------------------------------------------------------------

oneOf :: String -> Parser Char
oneOf s = satisfy (`elem` s)

notOneOf :: String -> Parser Char
notOneOf s = satisfy (notP (`elem` s))

char :: Char -> Parser Char
char c = satisfy (c ==)

string :: String -> Parser String
string [] = return []
string (c:cs) = do { char c; string cs; return (c:cs)}

token :: Parser a -> Parser a
token p = do { a <- p; spaces ; return a}

reserved :: String -> Parser String
reserved s = token (string s)

spaces :: Parser String
spaces = many $ oneOf " \t"

newline :: Parser Char
newline = char '\n'

squareBrackets :: Parser a -> Parser a
squareBrackets m = do
  reserved "["
  n <- m
  reserved "]"
  return n

data IniFile
  = DesktopEntrySection String
  | KeyValues [(String, String)]
  deriving Show

keyName :: Parser String
keyName = some (notOneOf "=\n \t")

keyValue :: Parser (String, String)
keyValue = do
  key <- keyName
  spaces
  char '='
  spaces
  val <- many (notOneOf "\n")
  newline
  return (key, val)

nonSectionLine :: Parser String
nonSectionLine = do
  startChar <- notOneOf "["
  otherChar <- many $ notOneOf "\n"
  newline
  return $ startChar : otherChar

desktopEntrySectionLine :: Parser (Either String String)
desktopEntrySectionLine = do
  sectionName <- squareBrackets (string "Desktop Entry")
  newline
  return $ Right sectionName

badSectionLine :: Parser (Either String String)
badSectionLine = do
  startChar <- char '['
  otherChar <- many $ notOneOf "\n"
  newline
  return $ Left $ startChar : otherChar

emptyLine :: Parser ()
emptyLine = do
  whitespaceLine <|> commentLine
  return ()
  where whitespaceLine = spaces >> newline
        commentLine = spaces
                   >> char '#'
                   >> many (notOneOf "\n")
                   >> newline

sectionBodyLine :: Parser (Maybe (String, String))
sectionBodyLine = (Just <$> keyValue)
                  <|> (Nothing <$ emptyLine)


section :: Parser (Either String (String, MAP.Map String String))
section = do
  many nonSectionLine
  sectionLabel <- desktopEntrySectionLine <|> badSectionLine
  keyValsList <- catMaybes <$> many sectionBodyLine
  let keyVals = MAP.fromList keyValsList
  return $ (,keyVals) <$> sectionLabel

dotDesktopParser :: Parser [Either String (String, MAP.Map String String)]
dotDesktopParser = do
  sections <- many section
  many nonSectionLine
  return sections

runDotDesktopParser :: String -> Either String [Either String (String, MAP.Map String String)]
runDotDesktopParser = runParser dotDesktopParser
