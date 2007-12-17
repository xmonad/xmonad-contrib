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
                                 googleSearch,
                                 wayback,
                                 waybackSelection,
                                 waybackSearch,
                                 wikipedia,
                                 wikipediaSelection,
                                 wikipediaSearch,
                                 promptSearch,
                                 search
                          ) where

import XMonad (io, X())
import XMonad.Util.Run (safeSpawn)
import XMonad.Prompt.Shell (getShellCompl)
import XMonad.Prompt (XPrompt(showXPrompt), mkXPrompt, XPConfig())
import XMonad.Util.XSelection (getSelection)
import Data.Char (chr, ord, isAlpha, isMark, isDigit)
import Numeric (showIntAtBase)

-- A customized prompt
data Search = Search
instance XPrompt Search where
    showXPrompt Search = "Search: "

-- | Escape the search string so search engines understand it.
-- Note that everything is escaped; we could be smarter and use 'isAllowedInURI'
-- but then that'd be hard enough to copy-and-paste we'd need to depend on 'network'.
escape :: String -> String
escape = escapeURIString (\c -> isAlpha c || isDigit c || isMark c)
         where
           escapeURIString ::
               (Char -> Bool)     -- ^ a predicate which returns 'False'
                               --   if the character should be escaped
               -> String           -- ^ the string to process
               -> String           -- ^ the resulting URI string
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
