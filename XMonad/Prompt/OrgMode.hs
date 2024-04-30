{-# LANGUAGE CPP                 #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData          #-}
{-# LANGUAGE ViewPatterns        #-}
--------------------------------------------------------------------
-- |
-- Module      :  XMonad.Prompt.OrgMode
-- Description :  A prompt for interacting with org-mode.
-- Copyright   :  (c) 2021  Tony Zorman
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  Tony Zorman <soliditsallgood@mailbox.org>
-- Stability   :  experimental
-- Portability :  unknown
--
-- A prompt for interacting with <https:\/\/orgmode.org\/ org-mode>.
-- This can be seen as an org-specific version of
-- "XMonad.Prompt.AppendFile", allowing for more interesting
-- interactions with that particular file type.
--
-- It can be used to quickly save TODOs, NOTEs, and the like with the
-- additional capability to schedule/deadline a task, add a priority,
-- refile to some existing heading, and use the system's clipboard
-- (really: the primary selection) as the contents of the note.
--
-- A blog post highlighting some features of this module can be found
-- <https://tony-zorman.com/posts/orgmode-prompt/2022-08-27-xmonad-and-org-mode.html here>.
--
--------------------------------------------------------------------
module XMonad.Prompt.OrgMode (
    -- * Usage
    -- $usage

    -- * Prompts
    orgPrompt,              -- :: XPConfig -> String -> FilePath -> X ()
    orgPromptRefile,        -- :: XPConfig -> [String] -> String -> FilePath -> X ()
    orgPromptRefileTo,      -- :: XPConfig -> String -> String -> FilePath -> X ()
    orgPromptPrimary,       -- :: XPConfig -> String -> FilePath -> X ()

    -- * Types
    ClipboardSupport (..),
    OrgMode,                -- abstract

#ifdef TESTING
    pInput,
    Note (..),
    Priority (..),
    Date (..),
    Time (..),
    TimeOfDay (..),
    OrgTime (..),
    DayOfWeek (..),
#endif

) where

import XMonad.Prelude

import XMonad (X, io, whenJust)
import XMonad.Prompt (XPConfig, XPrompt (showXPrompt), mkXPromptWithReturn, mkComplFunFromList, ComplFunction)
import XMonad.Util.Parser
import XMonad.Util.XSelection (getSelection)
import XMonad.Util.Run

import Control.DeepSeq (deepseq)
import qualified Data.List.NonEmpty as NE (head)
import Data.Time (Day (ModifiedJulianDay), NominalDiffTime, UTCTime (utctDay), addUTCTime, fromGregorian, getCurrentTime, nominalDay, toGregorian)
#if MIN_VERSION_time(1, 9, 0)
import Data.Time.Format.ISO8601 (iso8601Show)
#else
import Data.Time.Format (defaultTimeLocale, formatTime, iso8601DateFormat)
#endif
import GHC.Natural (Natural)
import System.IO (IOMode (AppendMode, ReadMode), hClose, hGetContents, openFile, withFile)

{- $usage

You can use this module by importing it, along with "XMonad.Prompt", in
your @xmonad.hs@

> import XMonad.Prompt
> import XMonad.Prompt.OrgMode (orgPrompt)

and adding an appropriate keybinding.  For example, using syntax from
"XMonad.Util.EZConfig":

> , ("M-C-o", orgPrompt def "TODO" "/home/me/org/todos.org")

This would create notes of the form @* TODO /my-message/@ in the
specified file.

You can also enter a relative path; in that case the file path will be
prepended with @$HOME@ or an equivalent directory.  I.e. instead of the
above you can write

> , ("M-C-o", orgPrompt def "TODO" "org/todos.org")
>                -- also possible: "~/org/todos.org"

There is also some scheduling and deadline functionality present.  This
may be initiated by entering @+s@ or @+d@—separated by at least one
whitespace character on either side—into the prompt, respectively.
Then, one may enter a date and (optionally) a time of day.  Any of the
following are valid dates, where brackets indicate optionality:

  - tod[ay]
  - tom[orrow]
  - /any weekday/
  - /any date of the form DD [MM] [YYYY]/

In the last case, the missing month and year will be filled out with the
current month and year.

For weekdays, we also disambiguate as early as possible; a simple @w@
will suffice to mean Wednesday, but @s@ will not be enough to say
Sunday.  You can, however, also write the full word without any
troubles.  Weekdays always schedule into the future; e.g., if today is
Monday and you schedule something for Monday, you will actually schedule
it for the /next/ Monday (the one in seven days).

The time is specified in the @HH:MM@ or @HHMM@ format.  The minutes may
be omitted, in which case we assume a full hour is specified.  It is also
possible to enter a time span using the syntax @HH:MM-HH:MM@ or @HH:MM+HH@.
In the former case, minutes may be omitted.

A few examples are probably in order.  Suppose we have bound the key
above, pressed it, and are now confronted with a prompt:

  - @hello +s today@ would create a TODO note with the header @hello@
    and would schedule that for today's date.

  - @hello +s today 12@ schedules the note for today at 12:00.

  - @hello +s today 12:30@ schedules it for today at 12:30.

  - @hello +d today 12:30@ works just like above, but creates a
    deadline.

  - @hello +d today 12:30-14:30@ works like the above, but gives the
    event a duration of two hours.  An alternative way to specify
    this would be @hello +d today 12:30+2@.

  - @hello +s thu@ would schedule the note for next thursday.

  - @hello +s 11@ would schedule it for the 11th of this month and this
    year.

  - @hello +s 11 jan 2013@ would schedule the note for the 11th of
    January 2013.

Note that, due to ambiguity concerns, years below @25@ result in
undefined parsing behaviour.  Otherwise, what should @message +s 11 jan
13@ resolve to—the 11th of january at 13:00 or the 11th of january in
the year 13?

There is basic support for alphabetic org-mode
<https:\/\/orgmode.org\/manual\/Priorities.html priorities>.
Simply append either @#A@, @#B@, or @#C@ (capitalisation is optional) to
the end of the note.  For example, one could write @"hello +s 11 jan
2013 #A"@ or @"hello #C"@.  Note that there has to be at least one
whitespace character between the end of the note and the chosen
priority.

There's also the possibility to take what's currently in the primary
selection and paste that as the content of the created note.  This is
especially useful when you want to quickly save a URL for later and
return to whatever you were doing before.  See the 'orgPromptPrimary'
prompt for that.

Finally, 'orgPromptRefile' and 'orgPromptRefileTo' provide support to
automatically
<https://orgmode.org/manual/Refile-and-Copy.html refile>
the generated item under a heading of choice.  For example, binding

> orgPromptRefile def "TODO" "todos.org"

to a key will first pop up an ordinary prompt that works exactly like
'orgPrompt', and then query the user for an already existing heading
(with completions) as provided by the @~/todos.org@ file.  If that
prompt is cancelled, the heading will appear in the org file as normal
(i.e., at the end of the file); otherwise, it gets refiled under the
selected heading.

-}

{- TODO

  - XMonad.Util.XSelection.getSelection is really, really horrible.  The
    plan would be to rewrite this in a way so it uses xmonad's
    connection to the X server.

  - Add option to explicitly use the system clipboard instead of the
    primary selection.

-}

------------------------------------------------------------------------
-- Prompt

data OrgMode = OrgMode
  { clpSupport :: ClipboardSupport
  , todoHeader :: String    -- ^ Will display like @* todoHeader @
  , orgFile    :: FilePath
  }

mkOrgCfg :: ClipboardSupport -> String -> FilePath -> X OrgMode
mkOrgCfg clp header fp = OrgMode clp header <$> mkAbsolutePath fp

-- | Whether we should use a clipboard and which one to use.
data ClipboardSupport
  = PrimarySelection
  | NoClpSupport

-- | How one should display the clipboard string.
data Clp
  = Header String  -- ^ In the header as a link: @* [[clp][message]]@
  | Body   String  -- ^ In the body as additional text: @* message \n clp@

instance XPrompt OrgMode where
  showXPrompt :: OrgMode -> String
  showXPrompt OrgMode{ todoHeader, orgFile, clpSupport } =
    mconcat ["Add ", todoHeader, clp, " to ", orgFile, ": "]
   where
    clp :: String = case clpSupport of
      NoClpSupport     -> ""
      PrimarySelection -> " + PS"

-- | Prompt for interacting with @org-mode@.
orgPrompt
  :: XPConfig  -- ^ Prompt configuration
  -> String    -- ^ What kind of note to create; will be displayed after
               --   a single @*@
  -> FilePath  -- ^ Path to @.org@ file, e.g. @home\/me\/todos.org@
  -> X ()
orgPrompt xpc = (void . mkOrgPrompt xpc =<<) .: mkOrgCfg NoClpSupport

-- | Like 'orgPrompt', but additionally make use of the primary
-- selection.  If it is a URL, then use an org-style link
-- @[[primary-selection][entered message]]@ as the heading.  Otherwise,
-- use the primary selection as the content of the note.
--
-- The prompt will display a little @+ PS@ in the window
-- after the type of note.
orgPromptPrimary :: XPConfig -> String -> FilePath -> X ()
orgPromptPrimary xpc = (void . mkOrgPrompt xpc =<<) .: mkOrgCfg PrimarySelection

-- | Internal type in order to generate a nice prompt in
-- 'orgPromptRefile' and 'orgPromptRefileTo'.
data RefilePrompt = Refile
instance XPrompt RefilePrompt where
  showXPrompt :: RefilePrompt -> String
  showXPrompt Refile = "Refile note to: "

-- | Like 'orgPrompt' (which see for the other arguments), but offer to
-- refile the entered note afterwards.
--
-- Note that refiling is done by shelling out to Emacs, hence an @emacs@
-- binary must be in @$PATH@.  One may customise this by following the
-- instructions in "XMonad.Util.Run#g:EDSL"; more specifically, by
-- changing the 'XMonad.Util.Run.emacs' field of
-- 'XMonad.Util.Run.ProcessConfig'.
orgPromptRefile :: XPConfig -> String -> FilePath -> X ()
orgPromptRefile xpc str fp = do
  orgCfg <- mkOrgCfg NoClpSupport str fp

  -- NOTE: Ideally we would just use System.IO.readFile' here
  -- (especially because it also reads everything strictly), but this is
  -- only available starting in base 4.15.x.
  fileContents <- io $ do
    handle   <- openFile (orgFile orgCfg) ReadMode
    contents <- hGetContents handle
    contents <$ (contents `deepseq` hClose handle)

  -- Save the entry as soon as possible.
  notCancelled <- mkOrgPrompt xpc orgCfg
  when notCancelled $
    -- If the user didn't cancel, try to parse the org file and offer to
    -- refile the entry if possible.
    whenJust (runParser pOrgFile fileContents) $ \headings ->
      mkXPromptWithReturn Refile xpc (completeHeadings headings) pure >>= \case
        Nothing     -> pure ()
        Just parent -> refile parent (orgFile orgCfg)
 where
  completeHeadings :: [Heading] -> ComplFunction
  completeHeadings = mkComplFunFromList xpc . map headingText

-- | Like 'orgPromptRefile', but with a fixed heading for refiling; no
-- prompt will appear to query for a target.
--
-- Heading names may omit tags, but generally need to be prefixed by the
-- correct todo keywords; e.g.,
--
-- > orgPromptRefileTo def "PROJECT Work" "TODO" "~/todos.org"
--
-- Will refile the created note @"TODO <text>"@ to the @"PROJECT Work"@
-- heading, even with the actual name is @"PROJECT Work
-- :work:other_tags:"@.  Just entering @"Work"@ will not work, as Emacs
-- doesn't recognise @"PROJECT"@ as an Org keyword by default (i.e. when
-- started in batch-mode).
orgPromptRefileTo
  :: XPConfig
  -> String     -- ^ Heading to refile the entry under.
  -> String
  -> FilePath
  -> X ()
orgPromptRefileTo xpc refileHeading str fp = do
  orgCfg       <- mkOrgCfg NoClpSupport str fp
  notCancelled <- mkOrgPrompt xpc orgCfg
  when notCancelled $ refile refileHeading (orgFile orgCfg)

-- | Create the actual prompt.  Returns 'False' when the input was
-- cancelled by the user (by, for example, pressing @Esc@ or @C-g@) and
-- 'True' otherwise.
mkOrgPrompt :: XPConfig -> OrgMode -> X Bool
mkOrgPrompt xpc oc@OrgMode{ todoHeader, orgFile, clpSupport } =
  isJust <$> mkXPromptWithReturn oc xpc (const (pure [])) appendNote
 where
  -- | Parse the user input, create an @org-mode@ note out of that and
  -- try to append it to the given file.
  appendNote :: String -> X ()
  appendNote input = io $ do
    clpStr <- case clpSupport of
      NoClpSupport     -> pure $ Body ""
      PrimarySelection -> do
        sel <- getSelection
        pure $ if   any (`isPrefixOf` sel) ["http://", "https://"]
               then Header sel
               else Body   $ "\n " <> sel

    withFile orgFile AppendMode . flip hPutStrLn
      <=< maybe (pure "") (ppNote clpStr todoHeader) . pInput
        $ input

------------------------------------------------------------------------
-- Refiling

-- | Let Emacs do the refiling, as this seems—and I know how this
-- sounds—more robust than trying to do it ad-hoc in this module.
refile :: String -> FilePath -> X ()
refile (asString -> parent) (asString -> fp) =
  proc $ inEmacs
     >-> asBatch
     >-> eval (progn [ "find-file" <> fp
                     , "end-of-buffer"
                     , "org-refile nil nil"
                         <> list [ parent
                                 , fp
                                 , "nil"
                                 , saveExcursion ["org-find-exact-headline-in-buffer"
                                                    <> parent]
                                 ]
                     , "save-buffer"
                     ])

------------------------------------------------------------------------
-- Time

-- | A 'Time' is a 'Date' with the possibility of having a specified
-- @HH:MM@ time.
data Time = Time
  { date :: Date
  , tod  :: Maybe OrgTime
  }
  deriving (Eq, Show)

-- | The time in HH:MM.
data TimeOfDay = HHMM Int Int
  deriving (Eq)

instance Show TimeOfDay where
  show :: TimeOfDay -> String
  show (HHMM h m) = pad h <> ":" <> pad m
   where
    pad :: Int -> String
    pad n = (if n <= 9 then "0" else "") <> show n

-- | The time—possibly as a span—in HH:MM format.
data OrgTime = MomentInTime TimeOfDay | TimeSpan TimeOfDay TimeOfDay
  deriving (Eq)

instance Show OrgTime where
  show :: OrgTime -> String
  show (MomentInTime tod)  = show tod
  show (TimeSpan tod tod') = show tod <> "-" <> show tod'

-- | Type for specifying exactly which day one wants.
data Date
  = Today
  | Tomorrow
  | Next DayOfWeek
    -- ^ This will __always__ show the next 'DayOfWeek' (e.g. calling
    -- 'Next Monday' on a Monday will result in getting the menu for the
    -- following Monday)
  | Date (Int, Maybe Int, Maybe Integer)
    -- ^ Manual date entry in the format DD [MM] [YYYY]
  deriving (Eq, Ord, Show)

toOrgFmt :: Maybe OrgTime -> Day -> String
toOrgFmt tod day =
  mconcat ["<", isoDay, " ", take 3 $ show (dayOfWeek day), time, ">"]
 where
  time   :: String = maybe "" ((' ' :) . show) tod
#if MIN_VERSION_time(1, 9, 0)
  isoDay :: String = iso8601Show day
#else
  isoDay :: String = formatTime defaultTimeLocale (iso8601DateFormat Nothing) day
#endif

-- | Pretty print a 'Date' and an optional time to reflect the actual
-- date.
ppDate :: Time -> IO String
ppDate Time{ date, tod } = do
  curTime <- getCurrentTime
  let curDay      = utctDay curTime
      (y, m, _)   = toGregorian curDay
      diffToDay d = diffBetween d (dayOfWeek curDay)

  pure . toOrgFmt tod $ case date of
    Today              -> curDay
    Tomorrow           -> utctDay $ addDays 1 curTime
    Next wday          -> utctDay $ addDays (diffToDay wday) curTime
    Date (d, mbM, mbY) -> fromGregorian (fromMaybe y mbY) (fromMaybe m mbM) d
 where
  -- | Add a specified number of days to a 'UTCTime'.
  addDays :: NominalDiffTime -> UTCTime -> UTCTime
    = addUTCTime . (* nominalDay)

  -- | Evil enum hackery.
  diffBetween :: DayOfWeek -> DayOfWeek -> NominalDiffTime
  diffBetween d cur  -- we want to jump to @d@
    | d == cur  = 7
    | otherwise = fromIntegral . abs $ (fromEnum d - fromEnum cur) `mod` 7

-- Old GHC versions don't have a @time@ library new enough to have
-- this, so replicate it here for the moment.

dayOfWeek :: Day -> DayOfWeek
dayOfWeek (ModifiedJulianDay d) = toEnum $ fromInteger $ d + 3

data DayOfWeek
  = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
  deriving (Eq, Ord, Show)

-- | \"Circular\", so for example @[Tuesday ..]@ gives an endless
-- sequence.  Also: 'fromEnum' gives [1 .. 7] for [Monday .. Sunday],
-- and 'toEnum' performs mod 7 to give a cycle of days.
instance Enum DayOfWeek where
  toEnum :: Int -> DayOfWeek
  toEnum i = case mod i 7 of
    0 -> Sunday
    1 -> Monday
    2 -> Tuesday
    3 -> Wednesday
    4 -> Thursday
    5 -> Friday
    _ -> Saturday

  fromEnum :: DayOfWeek -> Int
  fromEnum = \case
    Monday    -> 1
    Tuesday   -> 2
    Wednesday -> 3
    Thursday  -> 4
    Friday    -> 5
    Saturday  -> 6
    Sunday    -> 7

------------------------------------------------------------------------
-- Note

-- | An @org-mode@ style note.
data Note
  = Scheduled String Time Priority
  | Deadline  String Time Priority
  | NormalMsg String      Priority
  deriving (Eq, Show)

-- | An @org-mode@ style priority symbol[1]; e.g., something like
-- @[#A]@.  Note that this uses the standard org conventions: supported
-- priorities are @A@, @B@, and @C@, with @A@ being the highest.
-- Numerical priorities are not supported.
--
-- [1]: https://orgmode.org/manual/Priorities.html
data Priority = A | B | C | NoPriority
  deriving (Eq, Show)

-- | Pretty print a given 'Note'.
ppNote :: Clp -> String -> Note -> IO String
ppNote clp todo = \case
  Scheduled str time prio -> mkLine str "SCHEDULED: " (Just time) prio
  Deadline  str time prio -> mkLine str "DEADLINE: "  (Just time) prio
  NormalMsg str      prio -> mkLine str ""            Nothing     prio
 where
  mkLine :: String -> String -> Maybe Time -> Priority -> IO String
  mkLine str sched time prio = do
    t <- case time of
      Nothing -> pure ""
      Just ti -> (("\n  " <> sched) <>) <$> ppDate ti
    pure $ "* " <> todo <> priority <> case clp of
      Body   c -> mconcat [str, t, c]
      Header c -> mconcat ["[[", c, "][", str,"]]", t]
   where
    priority = case prio of
      NoPriority -> " "
      otherPrio  -> " [#" <> show otherPrio <> "] "

------------------------------------------------------------------------
-- Note parsing

-- | Parse the given string into a 'Note'.
pInput :: String -> Maybe Note
pInput inp = (`runParser` inp) . choice $
  [ Scheduled <$> (getLast "+s" <* " ") <*> join (fixTime <$> pDate <*> pOrgTime) <*> pPriority
  , Deadline  <$> (getLast "+d" <* " ") <*> join (fixTime <$> pDate <*> pOrgTime) <*> pPriority
  , do s <- munch1 (pure True)
       let (s', p) = splitAt (length s - 3) s
       pure $ case tryPrio p of
         Just prio -> NormalMsg (dropStripEnd 0 s') prio
         Nothing   -> NormalMsg s                   NoPriority
  ]
 where
  fixTime :: Maybe Date -> Maybe OrgTime -> Parser Time
  fixTime d tod = case (d, tod) of
    (Nothing, Nothing) -> mempty                -- no day and no time
    (Nothing, Just{})  -> pure (Time Today tod) -- no day, but a time
    (Just d', _)       -> pure (Time d'    tod) -- day given

  tryPrio :: String -> Maybe Priority
  tryPrio [' ', '#', x]
    | x `elem` ("Aa" :: String) = Just A
    | x `elem` ("Bb" :: String) = Just B
    | x `elem` ("Cc" :: String) = Just C
  tryPrio _ = Nothing

  -- Trim whitespace at the end of a string after dropping some number
  -- of characters from it.
  dropStripEnd :: Int -> String -> String
  dropStripEnd n = reverse . dropWhile (== ' ') . drop n . reverse

  getLast :: String -> Parser String
  getLast ptn =  dropStripEnd (length ptn) -- drop only the last pattern before stripping
              .  concat
             <$> endBy1 (go "") (pure ptn)
   where
    go :: String -> Parser String
    go consumed = do
      str  <- munch  (/= NE.head (notEmpty ptn))
      word <- munch1 (/= ' ')
      bool go pure (word == ptn) $ consumed <> str <> word

-- | Parse a 'Priority'.
pPriority :: Parser Priority
pPriority = option NoPriority $
  skipSpaces *> choice
    [ "#" *> foldCase "a" $> A
    , "#" *> foldCase "b" $> B
    , "#" *> foldCase "c" $> C
    ]

-- | Try to parse a 'Time'.
pOrgTime :: Parser (Maybe OrgTime)
pOrgTime = option Nothing $
  between skipSpaces (void " " <|> eof) $
    Just <$> choice
      [ TimeSpan <$> (pTimeOfDay <* ("--" <|> "-" <|> "–")) <*> pTimeOfDay
      -- Org is not super smart around times with this syntax, so
      -- we pretend not to be as well.
      , do from@(HHMM h m) <- pTimeOfDay <* "+"
           off <- pHour
           pure $ TimeSpan from (HHMM (h + off) m)
      , MomentInTime <$> pTimeOfDay
      ]
 where
  pTimeOfDay :: Parser TimeOfDay
  pTimeOfDay = choice
    [ HHMM <$> pHour <* ":" <*> pMinute -- HH:MM
    , pHHMM                             -- HHMM
    , HHMM <$> pHour        <*> pure 0  -- HH
    ]

  pHHMM :: Parser TimeOfDay
  pHHMM = do
    let getTwo = count 2 (satisfy isDigit)
    hh <- read <$> getTwo
    guard (hh >= 0 && hh <= 23)
    mm <- read <$> getTwo
    guard (mm >= 0 && mm <= 59)
    pure $ HHMM hh mm

  pHour   :: Parser Int = pNumBetween 0 23
  pMinute :: Parser Int = pNumBetween 0 59

-- | Try to parse a 'Date'.
pDate :: Parser (Maybe Date)
pDate = skipSpaces *> optional (choice
  [ pPrefix "tod" "ay"    Today
  , pPrefix "tom" "orrow" Tomorrow
  , Next <$> pNext
  , Date <$> pDate'
  ])
 where
  pNext :: Parser DayOfWeek = choice
    [ pPrefix "m"  "onday"    Monday   , pPrefix "tu" "esday"  Tuesday
    , pPrefix "w"  "ednesday" Wednesday, pPrefix "th" "ursday" Thursday
    , pPrefix "f"  "riday"    Friday   , pPrefix "sa" "turday" Saturday
    , pPrefix "su" "nday"     Sunday
    ]

  numWithoutColon :: Parser Int
  numWithoutColon = do
    str <- pNumBetween 1 12 -- month
    c <- get
    if c == ':'
    then pfail
    else pure str

  pDate' :: Parser (Int, Maybe Int, Maybe Integer)
  pDate' =
    (,,) <$> (pNumBetween 1 31 <* (void " " <|> eof))  -- day
         <*> optional (skipSpaces *> choice
               [ pPrefix "ja"  "nuary"    1 , pPrefix "f"   "ebruary" 2
               , pPrefix "mar" "ch"       3 , pPrefix "ap"  "ril"     4
               , pPrefix "may" ""         5 , pPrefix "jun" "e"       6
               , pPrefix "jul" "y"        7 , pPrefix "au"  "gust"    8
               , pPrefix "s"   "eptember" 9 , pPrefix "o"   "ctober"  10
               , pPrefix "n"   "ovember"  11, pPrefix "d"   "ecember" 12
               , numWithoutColon
               ])
         <*> optional (skipSpaces *> num >>= \i -> guard (i >= 25) $> i)

  -- Parse a prefix and drop a potential suffix up to the next (space
  -- separated) word.  If successful, return @ret@.
  pPrefix :: String -> String -> a -> Parser a
  pPrefix start (map toLower -> leftover) ret = do
    void (foldCase start)
    l <- map toLower <$> munch (/= ' ')
    guard (l `isPrefixOf` leftover)
    pure ret

-- | Parse a number between @lo@ (inclusive) and @hi@ (inclusive).
pNumBetween :: Int -> Int -> Parser Int
pNumBetween lo hi = do
  n <- num
  n <$ guard (n >= lo && n <= hi)

-- Parse the given string case insensitively.
foldCase :: String -> Parser String
foldCase = traverse (\c -> char (toLower c) <|> char (toUpper c))

------------------------------------------------------------------------
-- File parsing

data Heading = Heading
  { level       :: Natural
    -- ^ Level of the Org heading; i.e., the number of leading stars.
  , headingText :: String
    -- ^ The heading text without its level.
  }

-- | Naïvely parse an Org file.  At this point, only the headings are
-- parsed into a non-nested list (ignoring parent-child relations); no
-- further analysis is done on the individual lines themselves.
pOrgFile :: Parser [Heading]
pOrgFile = many pHeading

pHeading :: Parser Heading
pHeading = skipSpaces *> do
  level       <- genericLength <$> munch1 (== '*') <* " "
  headingText <- pLine
  void $ many (pLine >>= \line -> guard (isNotHeading line) $> line) -- skip body
  pure Heading{..}

pLine :: Parser String
pLine = munch (/= '\n') <* "\n"

isNotHeading :: String -> Bool
isNotHeading str = case break (/= '*') str of
  ("", _)       -> True
  (_ , ' ' : _) -> False
  _             -> True
