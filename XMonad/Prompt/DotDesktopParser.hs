{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
module XMonad.Prompt.DotDesktopParser
    ( doParseContent
    , DotDesktopApp (..)
    ) where

import Text.ParserCombinators.ReadP
    ( ReadP
    , (<++)
    , char
    , eof
    , many
    , many1
    , readP_to_S
    , satisfy
    , skipSpaces
    , string
    , between
    ,  (+++) )
import Data.Maybe ( listToMaybe, catMaybes, fromMaybe )
import qualified Data.Map as MAP
import Control.Monad ( (>=>), void )
import Data.Char ( isSpace )
import Data.List ( dropWhileEnd )

type Predicate a = a -> Bool

notP :: Predicate a -> Predicate a
notP = (not .)

notOneOf :: String -> ReadP Char
notOneOf s = satisfy (notP (`elem` s))

newline :: ReadP ()
newline = void (char '\n')

squareBrackets :: ReadP a -> ReadP a
squareBrackets = between (char '[') (char ']')

keyName :: ReadP String
keyName = many1 (notOneOf "=\n \t")

keyValue :: ReadP (String, String)
keyValue = do
  key <- keyName
  skipSpaces
  char '='
  skipSpaces
  val <- many (notOneOf "\n")
  char '\n'
  return (key, val)

desktopEntrySectionLine :: ReadP (Either String String)
desktopEntrySectionLine = do
  sectionName <- squareBrackets (string "Desktop Entry")
  newline
  return $ Right sectionName

badSectionLine :: ReadP (Either String String)
badSectionLine = do
  startChar <- char '['
  otherChar <- many $ notOneOf "\n"
  newline
  return $ Left $ startChar : otherChar

emptyLine :: ReadP ()
emptyLine = do
  whitespaceLine +++ commentLine +++ newline
  return ()
  where whitespaceLine = skipSpaces >> newline
        commentLine = skipSpaces
                   >> char '#'
                   >> many (notOneOf "\n")
                   >> newline

sectionBodyLine :: ReadP (Maybe (String, String))
sectionBodyLine = (Just <$> keyValue)
                  <++ (Nothing <$ emptyLine)

section :: ReadP (Either String (String, MAP.Map String String))
section = do
  many emptyLine
  sec <- desktopEntrySectionLine <++ badSectionLine
  keyValsList <- catMaybes <$> many sectionBodyLine
  let keyVals = MAP.fromList keyValsList
  return $ (,keyVals) <$> sec

dotDesktopReadP :: ReadP [Either String (String, MAP.Map String String)]
dotDesktopReadP = do
  sections <- many section
  eof
  return sections

runDotDesktopParser :: String -> Maybe (Either String (String, MAP.Map String String))
runDotDesktopParser = (listToMaybe . readP_to_S dotDesktopReadP) >=> (listToMaybe . fst)

maybeToEither :: b -> Maybe a -> Either b a
maybeToEither _ (Just a) = Right a
maybeToEither b Nothing = Left b

getVal :: String -> String
  -> MAP.Map String String -> Either String String
getVal msg k kvmap = maybeToEither msg $ MAP.lookup k kvmap

cmdFilter :: String -> String  -- fixme future do something other than dropping these
cmdFilter ('%':'f':xs) = cmdFilter xs
cmdFilter ('%':'F':xs) = cmdFilter xs
cmdFilter ('%':'u':xs) = cmdFilter xs
cmdFilter ('%':'U':xs) = cmdFilter xs
cmdFilter ('%':'c':xs) = cmdFilter xs
cmdFilter ('%':'k':xs) = cmdFilter xs
cmdFilter ('%':'i':xs) = cmdFilter xs
cmdFilter ('%':'%':xs) = '%' : cmdFilter xs
cmdFilter (x:xs) = x : cmdFilter xs
cmdFilter "" = ""

trimWhitespace :: String -> String
trimWhitespace = dropWhileEnd isSpace . dropWhile isSpace

doParseContent :: String -> String -> Either String DotDesktopApp
doParseContent filePath content = do
  parsed <- fromMaybe
            (Left $ "Parse Resulted in no KeyVals in file " ++ filePath)
            (runDotDesktopParser content)
  let keyVals = snd parsed
  let errMsg = "Unable to find Name in file " ++ filePath
  nom <- getVal errMsg "Name" keyVals
  exc <- getVal errMsg "Exec" keyVals
  typ <- getVal errMsg "Type" keyVals
  return DotDesktopApp { fileName = filePath
                       , name = nom
                       , type_ = typ
                       , exec = exc
                       , cmd = (trimWhitespace . cmdFilter) exc
                       }

data DotDesktopApp = DotDesktopApp { fileName :: String
                             , name :: String
                             , type_ :: String
                             , exec :: String
                             , cmd :: String
                             } deriving Show

