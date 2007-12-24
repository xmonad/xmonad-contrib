{- |
 Module      :  XMonad.Actions.Search
 Copyright   :  (C) 2007 Gwern Branwen
 License     :  None; public domain

 Maintainer  :  <gwern0@gmail.com>
 Stability   :  unstable
 Portability :  unportable

 A module for easily running Internet searches on web sites through XMonad.
 Modeled after the handy Surfraw CLI search tools
 <https://secure.wikimedia.org/wikipedia/en/wiki/Surfraw>.

 Additional sites welcomed.
--------------------------------------------------------------------------- -}
module XMonad.Actions.Search (      -- * Usage
                                 -- $usage
                                 search,
                                 promptSearch,
                                 selectSearch,

                                 amazon,
                                 google,
                                 imdb,
                                 wayback,
                                 wikipedia,
                                 hoogle
                          ) where

import Data.Char (chr, ord, isAlpha, isMark, isDigit)
import Numeric (showIntAtBase)
import XMonad (X(), MonadIO)
import XMonad.Prompt (XPrompt(showXPrompt), mkXPrompt, XPConfig())
import XMonad.Prompt.Shell (getShellCompl)
import XMonad.Util.Run (safeSpawn)
import XMonad.Util.XSelection (getSelection)

-- A customized prompt.
data Search = Search
instance XPrompt Search where
    showXPrompt Search = "Search: "

-- | Escape the search string so search engines understand it.
-- Note that everything is escaped; we could be smarter and use 'isAllowedInURI'
-- but then that'd be hard enough to copy-and-paste we'd need to depend on 'network'.
escape :: String -> String
escape = escapeURIString (\c -> isAlpha c || isDigit c || isMark c)
         where -- Copied from Network.URI.
           escapeURIString ::
               (Char -> Bool)      -- a predicate which returns 'False' if should escape
               -> String           -- the string to process
               -> String           -- the resulting URI string
           escapeURIString p s = concatMap (escapeURIChar p) s
           escapeURIChar :: (Char->Bool) -> Char -> String
           escapeURIChar p c
               | p c       = [c]
               | otherwise = '%' : myShowHex (ord c) ""
               where
                 myShowHex :: Int -> ShowS
                 myShowHex n r =  case showIntAtBase 16 (toChrHex) n r of
                                    []  -> "00"
                                    [ch] -> ['0',ch]
                                    cs  -> cs
                 toChrHex d
                   | d < 10    = chr (ord '0' + fromIntegral d)
                   | otherwise = chr (ord 'A' + fromIntegral (d - 10))

type Browser      = FilePath
type SearchEngine = String -> String

search :: MonadIO m => Browser -> SearchEngine -> String -> m ()
search browser site query = safeSpawn browser $ site query

-- | Given a base URL, create the SearchEngine that escapes the query and
-- appends it to the base
simpleEngine :: String -> SearchEngine
simpleEngine site query = site ++ escape query

-- The engines
amazon, google, imdb, wayback, wikipedia, hoogle :: SearchEngine
amazon    = simpleEngine "http://www.amazon.com/exec/obidos/external-search?index=all&keyword="
google    = simpleEngine "http://www.google.com/search?num=100&q="
imdb      = simpleEngine "http://www.imdb.com/Find?select=all&for="
wikipedia = simpleEngine "https://secure.wikimedia.org/wikipedia/en/wiki/Special:Search?go=Go&search="
wayback   = simpleEngine "http://web.archive.org/"
{- This doesn't seem to work, but nevertheless, it seems to be the official
   method at <http://web.archive.org/collections/web/advanced.html> to get the
   latest backup. -}
hoogle    = simpleEngine "http://www.haskell.org/hoogle/?q="

-- | Like 'search', but in this case, the string is not specified but grabbed
-- from the user's response to a prompt.
promptSearch :: XPConfig -> Browser -> SearchEngine -> X ()
promptSearch config browser site = mkXPrompt Search config (getShellCompl []) $ search browser site

-- | Like search, but for use with the X selection; it grabs the selection,
-- passes it to a given searchEngine and opens it in a browser. The various
-- *Selection functions specialize this to a particular search engine to make
-- things easier.
selectSearch :: MonadIO m => Browser -> SearchEngine -> m ()
selectSearch browser searchEngine = search browser searchEngine =<< getSelection
