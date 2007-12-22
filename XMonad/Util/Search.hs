{- |
 Module      :  XMonad.Util.Search
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
module XMonad.Util.Search (      -- * Usage
                                 -- $usage
                                 amazon, amazonSelection, amazonSearch,
                                 google, googleSelection, googleSearch,
                                 imdb, imdbSelection, imdbSearch,
                                 wayback, waybackSelection, waybackSearch,
                                 wikipedia, wikipediaSelection, wikipediaSearch,
                                 promptSearch,
                                 search
                          ) where

import Control.Monad.Trans (MonadIO()) -- for select's type signature
import Data.Char (chr, ord, isAlpha, isMark, isDigit)
import Numeric (showIntAtBase)
import XMonad (io, X())
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

-- | Given the base search URL, a browser to use, and the actual query, escape
-- the query, prepend the base URL, and hand it off to the browser.
search :: String -> FilePath -> String -> IO ()
search site browser query = safeSpawn browser $ site ++ escape query

-- The engines
amazonSearch, googleSearch, imdbSearch, waybackSearch, wikipediaSearch :: String -> String -> IO ()
amazonSearch    = search "http://www.amazon.com/exec/obidos/external-search?index=all&keyword="
googleSearch    = search "http://www.google.com/search?num=100&q="
imdbSearch      = search "http://www.imdb.com/Find?select=all&for="
wikipediaSearch = search "https://secure.wikimedia.org/wikipedia/en/wiki/Special:Search?go=Go&search="
waybackSearch   = search "http://web.archive.org/"
{- This doesn't seem to work, but nevertheless, it seems to be the official
   method at <http://web.archive.org/collections/web/advanced.html> to get the
   latest backup. -}

-- | Like 'search', but in this case, the string is not specified but grabbed
-- from the user's response to a prompt.
promptSearch :: (String -> String -> IO ()) -> String -> XPConfig -> X ()
promptSearch searchEngine browser config = mkXPrompt Search config (getShellCompl []) $ io . (searchEngine browser)

-- | Search the particular site; these are suitable for binding to a key. Use them like this:
--
-- > , ((modm,               xK_g     ), google "firefox" defaultXPConfig)
--
-- First argument is the browser you want to use, the second the prompt configuration.
amazon, google, imdb, wayback, wikipedia :: String -> XPConfig -> X ()
amazon    = promptSearch amazonSearch
google    = promptSearch googleSearch
imdb      = promptSearch imdbSearch
wikipedia = promptSearch wikipediaSearch
wayback   = promptSearch waybackSearch

-- | Like search, but for use with the X selection; it grabs the selection,
-- passes it to a given searchEngine and opens it in a browser. The various
-- *Selection functions specialize this to a particular search engine to make
-- things easier.
select :: (Control.Monad.Trans.MonadIO m) => (t -> String -> IO a) -> t -> m a
select browser searchEngine = io $ browser searchEngine =<< getSelection

-- | Like the google\/wikipedia functions, but one less argument - the query is
-- extracted from the copy-paste buffer of X Windows.
amazonSelection, googleSelection, imdbSelection, waybackSelection, wikipediaSelection :: String -> X ()
amazonSelection    = select amazonSearch
googleSelection    = select googleSearch
imdbSelection      = select imdbSearch
wikipediaSelection = select wikipediaSearch
waybackSelection   = select waybackSearch
