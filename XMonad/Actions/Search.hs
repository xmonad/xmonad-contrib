{- | Module      :  XMonad.Actions.Search
   Copyright   :  (C) 2007 Gwern Branwen
   License     :  None; public domain

   Maintainer  :  <gwern0@gmail.com>
   Stability   :  unstable
   Portability :  unportable; depends on XSelection, XPrompt

   A module for easily running Internet searches on web sites through xmonad.
   Modeled after the handy Surfraw CLI search tools at <https://secure.wikimedia.org/wikipedia/en/wiki/Surfraw>.

   Additional sites welcomed. -}
module XMonad.Actions.Search (   -- * Usage
                                 -- $usage
                                 search,
                                 SearchEngine(..),
                                 searchEngine,
                                 searchEngineF,
                                 promptSearch,
                                 promptSearchBrowser,
                                 selectSearch,
                                 selectSearchBrowser,
                                 hasPrefix,
                                 escape,
                                 use,
                                 intelligent,
                                 (!>),
                                 prefixAware,
                                 namedEngine,

                                 amazon,
                                 alpha,
                                 codesearch,
                                 deb,
                                 debbts,
                                 debpts,
                                 dictionary,
                                 google,
                                 hackage,
                                 hoogle,
                                 images,
                                 imdb,
                                 isohunt,
                                 lucky,
                                 maps,
                                 mathworld,
                                 scholar,
                                 thesaurus,
                                 wayback,
                                 wikipedia,
                                 youtube,
                                 multi
                                  -- * Use case: searching with a submap
                                  -- $tip
                          ) where

import Data.Char (chr, ord, isAlpha, isMark, isDigit)
import Numeric (showIntAtBase)
import XMonad (X(), MonadIO, liftIO)
import XMonad.Prompt (XPrompt(showXPrompt), mkXPrompt, XPConfig(), historyCompletion)
import XMonad.Prompt.Shell (getBrowser)
import XMonad.Util.Run (safeSpawn)
import XMonad.Util.XSelection (getSelection)

{- $usage

   This module is intended to allow easy access to databases on the
   Internet through xmonad's interface. The idea is that one wants to
   run a search but the query string and the browser to use must come
   from somewhere. There are two places the query string can come from
   - the user can type it into a prompt which pops up, or the query
   could be available already in the X Windows copy\/paste buffer
   (perhaps you just highlighted the string of interest).

   Thus, there are two main functions: 'promptSearch', and
   'selectSearch' (implemented using the more primitive 'search'). To
   each of these is passed an engine function; this is a function that
   knows how to search a particular site.

   For example, the 'google' function knows how to search Google, and
   so on. You pass 'promptSearch' and 'selectSearch' the engine you
   want, the browser you want, and anything special they might need;
   this whole line is then bound to a key of you choosing in your
   xmonad.hs. For specific examples, see each function.  This module
   is easily extended to new sites by using 'searchEngine'.

   The currently available search engines are:

* 'amazon' -- Amazon keyword search.

* 'alpha' -- Wolfram|Alpha query.

* 'codesearch' -- Google Labs Code Search search.

* 'deb'    -- Debian package search.

* 'debbts' -- Debian Bug Tracking System.

* 'debpts'  -- Debian Package Tracking System.

* 'dictionary' -- dictionary.reference.com search.

* 'google' -- basic Google search.

* 'hackage' -- Hackage, the Haskell package database.

* 'hoogle' -- Hoogle, the Haskell libraries API search engine.

* 'images' -- Google images.

* 'imdb'   -- the Internet Movie Database.

* 'isohunt' -- isoHunt search.

* 'lucky' -- Google "I'm feeling lucky" search.

* 'maps'   -- Google maps.

* 'mathworld' -- Wolfram MathWorld search.

* 'scholar' -- Google scholar academic search.

* 'thesaurus' -- thesaurus.reference.com search.

* 'wayback' -- the Wayback Machine.

* 'wikipedia' -- basic Wikipedia search.

* 'youtube' -- Youtube video search.

* 'multi' -- Search based on the prefix. \"amazon:Potter\" will use amazon, etc. With no prefix searches google.

Feel free to add more! -}

{- $tip

In combination with "XMonad.Actions.Submap" you can create a powerful
and easy way to search without adding a whole bunch of bindings.

First import the necessary modules:

> import qualified XMonad.Prompt         as P
> import qualified XMonad.Actions.Submap as SM
> import qualified XMonad.Actions.Search as S

Then add the following to your key bindings:

> ...
> -- Search commands
> , ((modm, xK_s), SM.submap $ searchEngineMap $ S.promptSearch P.defaultXPConfig)
> , ((modm .|. shiftMask, xK_s), SM.submap $ searchEngineMap $ S.selectSearch)
>
> ...
>
> searchEngineMap method = M.fromList $
>       [ ((0, xK_g), method S.google)
>       , ((0, xK_h), method S.hoogle)
>       , ((0, xK_w), method S.wikipedia)
>       ]

Or in combination with XMonad.Util.EZConfig:

> ...
> ] -- end of regular keybindings
> -- Search commands
> ++ [("M-s " ++ k, S.promptSearch P.defaultXPConfig f) | (k,f) <- searchList ]
> ++ [("M-S-s " ++ k, S.selectSearch f) | (k,f) <- searchList ]
>
> ...
>
> searchList :: [(String, S.SearchEngine)]
> searchList = [ ("g", S.google)
>              , ("h", S.hoohle)
>              , ("w", S.wikipedia)
>              ]

Make sure to set firefox to open new pages in a new window instead of
in a new tab: @Firefox -> Edit -> Preferences -> Tabs -> New pages
should be opened in...@

Now /mod-s/ + /g/\//h/\//w/ prompts you for a search string, then
opens a new firefox window that performs the search on Google, Hoogle
or Wikipedia respectively.

If you select something in whatever application and hit /mod-shift-s/ +
/g/\//h/\//w/ it will search the selected string with the specified
engine.

Happy searching! -}

-- | A customized prompt indicating we are searching, and the name of the site.
data Search = Search Name
instance XPrompt Search where
    showXPrompt (Search name)= "Search [" ++ name ++ "]: "

-- | Escape the search string so search engines understand it.
-- Note that everything is escaped; we could be smarter and use 'isAllowedInURI'
-- but then that'd be hard enough to copy-and-paste we'd need to depend on @network@.
escape :: String -> String
escape = escapeURIString (\c -> isAlpha c || isDigit c || isMark c)
         where -- Copied from Network.URI.
           escapeURIString ::
               (Char -> Bool)      -- a predicate which returns 'False' if should escape
               -> String           -- the string to process
               -> String           -- the resulting URI string
           escapeURIString = concatMap . escapeURIChar
           escapeURIChar :: (Char->Bool) -> Char -> String
           escapeURIChar p c
               | p c       = [c]
               | otherwise = '%' : myShowHex (ord c) ""
               where
                 myShowHex :: Int -> ShowS
                 myShowHex n r =  case showIntAtBase 16 toChrHex n r of
                                    []  -> "00"
                                    [ch] -> ['0',ch]
                                    cs  -> cs
                 toChrHex d
                   | d < 10    = chr (ord '0' + fromIntegral d)
                   | otherwise = chr (ord 'A' + fromIntegral (d - 10))

type Browser      = FilePath
type Query        = String
type Site         = String -> String
type Name         = String
data SearchEngine = SearchEngine Name Site

-- | Given an already defined search engine, extracts its transformation
--   function, making it easy to create compound search engines.
--   For an instance you can use @use google@ to get a function which
--   makes the same transformation as the google search engine would.
use :: SearchEngine -> Site
use (SearchEngine _ engine) = engine

-- | Given a browser, a search engine's transformation function, and a search term, perform the
--   requested search in the browser.
search :: Browser -> Site -> Query -> X ()
search browser site query = safeSpawn browser [site query]

{- | Given a base URL, create the 'SearchEngine' that escapes the query and
   appends it to the base. You can easily define a new engine locally using
   exported functions without needing to modify "XMonad.Actions.Search":

> myNewEngine = searchEngine "site" "http://site.com/search="

   The important thing is that the site has a interface which accepts the escaped query
   string as part of the URL. Alas, the exact URL to feed searchEngine varies
   from site to site, often considerably, so there\'s no general way to cover this.

   Generally, examining the resultant URL of a search will allow you to reverse-engineer
   it if you can't find the necessary URL already described in other projects such as Surfraw. -}
searchEngine :: Name -> String -> SearchEngine
searchEngine name site = searchEngineF name (\s -> site ++ (escape s))

{- | If your search engine is more complex than this (you may want to identify
   the kind of input and make the search URL dependent on the input or put the query
   inside of a URL instead of in the end) you can use the alternative 'searchEngineF' function.

> searchFunc :: String -> String
> searchFunc s | s `hasPrefix` "wiki:"   = "http://en.wikipedia.org/wiki/" ++ (escape $ tail $ snd $ break (==':') s)
>              | s `hasPrefix` "http://" = s
>              | otherwise               = (use google) s
> myNewEngine = searchEngineF "mymulti" searchFunc

   @searchFunc@ here searches for a word in wikipedia if it has a prefix
   of \"wiki:\" (you can use the 'escape' function to escape any forbidden characters), opens an address
   directly if it starts with \"http:\/\/\" and otherwise uses the provided google search engine.
   You can use other engines inside of your own through the 'use' function as shown above to make
   complex searches.

   The user input will be automatically escaped in search engines created with 'searchEngine',
   'searchEngineF', however, completely depends on the transformation function passed to it. -}
searchEngineF :: Name -> Site -> SearchEngine
searchEngineF = SearchEngine


-- The engines.
amazon, alpha, codesearch, deb, debbts, debpts, dictionary, google, hackage, hoogle,
  images, imdb, isohunt, lucky, maps, mathworld, scholar, thesaurus, wayback, wikipedia,
  youtube :: SearchEngine
amazon     = searchEngine "amazon"     "http://www.amazon.com/exec/obidos/external-search?index=all&keyword="
alpha      = searchEngine "alpha"      "http://www.wolframalpha.com/input/?i="
codesearch = searchEngine "codesearch" "http://www.google.com/codesearch?q="
deb        = searchEngine "deb"        "http://packages.debian.org/"
debbts     = searchEngine "debbts"     "http://bugs.debian.org/"
debpts     = searchEngine "debpts"     "http://packages.qa.debian.org/"
dictionary = searchEngine "dict"       "http://dictionary.reference.com/browse/"
google     = searchEngine "google"     "http://www.google.com/search?num=100&q="
hackage    = searchEngine "hackage"    "http://hackage.haskell.org/package/"
hoogle     = searchEngine "hoogle"     "http://www.haskell.org/hoogle/?q="
images     = searchEngine "images"     "http://images.google.fr/images?q="
imdb       = searchEngine "imdb"       "http://www.imdb.com/Find?select=all&for="
isohunt    = searchEngine "isohunt"    "http://isohunt.com/torrents/?ihq="
lucky      = searchEngine "lucky"      "http://www.google.com/search?btnI&q="
maps       = searchEngine "maps"       "http://maps.google.com/maps?q="
mathworld  = searchEngine "mathworld"  "http://mathworld.wolfram.com/search/?query="
scholar    = searchEngine "scholar"    "http://scholar.google.com/scholar?q="
thesaurus  = searchEngine "thesaurus"  "http://thesaurus.reference.com/search?q="
wikipedia  = searchEngine "wiki"       "https://secure.wikimedia.org/wikipedia/en/wiki/Special:Search?go=Go&search="
youtube    = searchEngine "youtube"    "http://www.youtube.com/results?search_type=search_videos&search_query="
{- This doesn't seem to work, but nevertheless, it seems to be the official
   method at <http://web.archive.org/collections/web/advanced.html> to get the
   latest backup. -}
wayback   = searchEngine "wayback" "http://web.archive.org/"

multi :: SearchEngine
multi = namedEngine "multi" $ foldr1 (!>) [amazon, alpha, codesearch, deb, debbts, debpts, dictionary, google, hackage, hoogle, images, imdb, isohunt, lucky, maps, mathworld, scholar, thesaurus, wayback, wikipedia, (prefixAware google)]

{- | This function wraps up a search engine and creates a new one, which works
   like the argument, but goes directly to a URL if one is given rather than
   searching.

> myIntelligentGoogleEngine = intelligent google

   Now if you search for http:\/\/xmonad.org it will directly open in your browser-}
intelligent :: SearchEngine -> SearchEngine
intelligent (SearchEngine name site) = searchEngineF name (\s -> if (fst $ break (==':') s) `elem` ["http", "https", "ftp"] then s else (site s))

{- | Checks if a string starts with a given prefix -}
hasPrefix :: String -> String -> Bool
hasPrefix _ [] = True
hasPrefix [] (_:_) = False
hasPrefix (t:ts) (p:ps) = if t == p then hasPrefix ts ps else False

removeColonPrefix :: String -> String
removeColonPrefix str = tail $ snd $ break (==':') str

{- | Connects a few search engines into one. If the search engines\' names are
   \"s1\", \"s2\" and \"s3\", then the resulting engine will use s1 if the query
   is @s1:word@, s2 if you type @s2:word@ and s3 in all other cases.

   Example:

> multiEngine = intelligent (wikipedia !> mathworld !> (prefixAware google))

  Now if you type \"wiki:Haskell\" it will search for \"Haskell\" in Wikipedia,
  \"mathworld:integral\" will search mathworld, and everything else will fall back to
  google. The use of intelligent will make sure that URLs are opened directly. -}
(!>) :: SearchEngine -> SearchEngine -> SearchEngine
(SearchEngine name1 site1) !> (SearchEngine name2 site2) = searchEngineF (name1 ++ "/" ++ name2) (\s -> if s `hasPrefix` (name1++":") then site1 (removeColonPrefix s) else site2 s)

{- | Makes a search engine prefix-aware. Especially useful together with '!>'.
   It will automatically remove the prefix from a query so that you don\'t end
     up searching for google:xmonad if google is your fallback engine and you
     explicitly add the prefix. -}
prefixAware :: SearchEngine -> SearchEngine
prefixAware (SearchEngine name site) = SearchEngine name (\s -> if s `hasPrefix` (name++":") then site $ removeColonPrefix s else site s)

{- | Changes search engine's name -}
namedEngine :: Name -> SearchEngine -> SearchEngine
namedEngine name (SearchEngine _ site) = searchEngineF name site

{- | Like 'search', but for use with the output from a Prompt; it grabs the
   Prompt's result, passes it to a given searchEngine and opens it in a given
   browser. -}
promptSearchBrowser :: XPConfig -> Browser -> SearchEngine -> X ()
promptSearchBrowser config browser (SearchEngine name site) = mkXPrompt (Search name) config historyCompletion $ search browser site

{- | Like 'search', but in this case, the string is not specified but grabbed
 from the user's response to a prompt. Example:

> , ((modm, xK_g), promptSearch greenXPConfig google)

   This specializes "promptSearchBrowser" by supplying the browser argument as
   supplied by 'getBrowser' from "XMonad.Prompt.Shell". -}
promptSearch :: XPConfig -> SearchEngine -> X ()
promptSearch config engine = liftIO getBrowser >>= \ browser -> promptSearchBrowser config browser engine

-- | Like 'search', but for use with the X selection; it grabs the selection,
--   passes it to a given searchEngine and opens it in a given browser.
selectSearchBrowser :: Browser -> SearchEngine -> X ()
selectSearchBrowser browser (SearchEngine _ site) = search browser site =<< getSelection

{- | Like 'search', but for use with the X selection; it grabs the selection,
   passes it to a given searchEngine and opens it in the default browser . Example:

> , ((modm .|. shiftMask, xK_g), selectSearch google)

   This specializes "selectSearchBrowser" by supplying the browser argument as
   supplied by 'getBrowser' from "XMonad.Prompt.Shell". -}
selectSearch :: SearchEngine -> X ()
selectSearch engine = liftIO getBrowser >>= \browser -> selectSearchBrowser browser engine
