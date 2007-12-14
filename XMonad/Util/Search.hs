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
                                 google,
                                 googleSelection,
                                 wayback,
                                 waybackSelection,
                                 wikipedia,
                                 wikipediaSelection,
                                 promptSearch,
                                 search
                          ) where

import Data.Char (isAlpha, isDigit, isMark)
import XMonad (io, X())
import XMonad.Util.Run (safeSpawn)
import Network.URI (escapeURIString)
import XMonad.Prompt.Shell (getShellCompl)
import XMonad.Prompt (XPrompt(showXPrompt), mkXPrompt, XPConfig())
import XMonad.Util.XSelection (getSelection)

-- A customized prompt
data Search = Search
instance XPrompt Search where
    showXPrompt Search = "Search: "

-- | Escape the search string so search engines understand it.
-- We could just go (const False) and escape anything that even looks at us
-- funny, but that produces obfuscated search queries. So we merely escape
-- anything that doesn't look unfunny.
escape :: String -> String
escape = escapeURIString (\c -> isAlpha c || isDigit c || isMark c)

-- | Given the base search URL, a browser to use, and the actual query, escape
-- the query, prepend the base URL, and hand it off to the browser.
search :: String -> FilePath -> String -> IO ()
search site browser query = safeSpawn browser $ site ++ escape query

promptSearch :: (String -> String -> IO ()) -> String -> XPConfig -> X ()
promptSearch engine browser config = mkXPrompt Search config (getShellCompl []) $ io . (engine browser)

-- The engines
googleSearch, waybackSearch, wikipediaSearch :: String -> String -> IO ()
googleSearch    = search "http://www.google.com/search?num=100&q="
wikipediaSearch = search "https://secure.wikimedia.org/wikipedia/en/wiki/Special:Search?go=Go&search="
{- This doesn't seem to work, but nevertheless, it seems to be the official
   method at <http://web.archive.org/collections/web/advanced.html> to get the
   latest backup. -}
waybackSearch = search "http://web.archive.org/"

-- | Search the particular site; these are suitable for binding to a key. Use them like this:
--
-- > , ((modm,               xK_g     ), google "firefox" defaultXPConfig)
--
-- First argument is the browser you want to use, the second the prompt configuration
google, wayback, wikipedia :: String -> XPConfig -> X ()
google    = promptSearch googleSearch
wikipedia = promptSearch wikipediaSearch
wayback   = promptSearch waybackSearch

-- | See previous. Like google\/wikipedia, but one less argument - the query is
-- extracted from the copy-paste buffer of X Windows.
googleSelection, waybackSelection, wikipediaSelection :: String -> X ()
googleSelection browser    = io $ googleSearch browser =<< getSelection
wikipediaSelection browser = io $ wikipediaSearch browser =<< getSelection
waybackSelection browser   = io $ waybackSearch browser =<< getSelection
