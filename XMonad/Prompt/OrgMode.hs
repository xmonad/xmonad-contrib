{-# LANGUAGE CPP                 #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE StrictData          #-}
{-# LANGUAGE ScopedTypeVariables #-}
--------------------------------------------------------------------
-- |
-- Module      :  XMonad.Prompt.OrgMode
-- Description :  A prompt for interacting with org-mode.
-- Copyright   :  (c) 2021  slotThe <soliditsallgood@mailbox.org>
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  slotThe <soliditsallgood@mailbox.org>
-- Stability   :  experimental
-- Portability :  unknown
--
-- A prompt for interacting with <https:\/\/orgmode.org\/ org-mode>.
-- This can be seen as an org-specific version of
-- "XMonad.Prompt.AppendFile", allowing for more interesting
-- interactions with that particular file type.
--
-- It can be used to quickly save TODOs, NOTEs, and the like with
-- the additional capability to schedule/deadline a task, or use
-- the system's clipboard (really: the primary selection) as the
-- contents of the note.
--
--------------------------------------------------------------------
module XMonad.Prompt.OrgMode (
    -- * Usage
    -- $usage

    -- * Prompts
    orgPrompt,              -- :: XPConfig -> String -> FilePath -> X ()
    orgPromptPrimary,       -- :: XPConfig -> String -> FilePath -> X ()

    -- * Types
    ClipboardSupport (..),
    OrgMode,                -- abstract

#ifdef TESTING
    pInput,
    Note (..),
    Date (..),
    Time (..),
    TimeOfDay (..),
    DayOfWeek (..),
#endif

) where

import XMonad.Prelude

import XMonad (X, io)
import XMonad.Prompt (XPConfig, XPrompt (showXPrompt), mkXPrompt)
import XMonad.Util.Parser
import XMonad.Util.XSelection (getSelection)

import Data.Time (Day (ModifiedJulianDay), NominalDiffTime, UTCTime (utctDay), addUTCTime, defaultTimeLocale, formatTime, fromGregorian, getCurrentTime, iso8601DateFormat, nominalDay, toGregorian)
import System.Directory (getHomeDirectory)
import System.IO (IOMode (AppendMode), hPutStrLn, withFile)

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

The time is specified in the @HH:MM@ format.  The minutes may be
omitted, in which case we assume a full hour is specified.

A few examples are probably in order.  Suppose we have bound the key
above, pressed it, and are now confronted with a prompt:

  - @hello +s today@ would create a TODO note with the header @hello@
    and would schedule that for today's date.

  - @hello +s today 12@ schedules the note for today at 12:00.

  - @hello +s today 12:30@ schedules it for today at 12:30.

  - @hello +d today 12:30@ works just like above, but creates a
    deadline.

  - @hello +s thu@ would schedule the note for next thursday.

  - @hello +s 11@ would schedule it for the 11th of this month and this
    year.

  - @hello +s 11 jan 2013@ would schedule the note for the 11th of
    January 2013.

Note that, due to ambiguity concerns, years below @25@ result in
undefined parsing behaviour.  Otherwise, what should @message +s 11 jan
13@ resolve to—the 11th of january at 13:00 or the 11th of january in
the year 13?

There's also the possibility to take what's currently in the primary
selection and paste that as the content of the created note.  This is
especially useful when you want to quickly save a URL for later and
return to whatever you were doing before.  See the 'orgPromptPrimary'
prompt for that.

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
orgPrompt xpc = mkOrgPrompt xpc .: OrgMode NoClpSupport

-- | Like 'orgPrompt', but additionally make use of the primary
-- selection.  If it is a URL, then use an org-style link
-- @[[primary-selection][entered message]]@ as the heading.  Otherwise,
-- use the primary selection as the content of the note.
--
-- The prompt will display a little @+ PS@ in the window
-- after the type of note.
orgPromptPrimary :: XPConfig -> String -> FilePath -> X ()
orgPromptPrimary xpc = mkOrgPrompt xpc .: OrgMode PrimarySelection

-- | Create the actual prompt.
mkOrgPrompt :: XPConfig -> OrgMode -> X ()
mkOrgPrompt xpc oc@OrgMode{ todoHeader, orgFile, clpSupport } =
  mkXPrompt oc xpc (const (pure [])) appendNote
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

    -- Expand path if applicable
    fp <- case orgFile of
      '/'       : _ -> pure orgFile
      '~' : '/' : _ -> getHomeDirectory <&> (<> drop 1 orgFile)
      _             -> getHomeDirectory <&> (<> ('/' : orgFile))

    withFile fp AppendMode . flip hPutStrLn
      <=< maybe (pure "") (ppNote clpStr todoHeader) . pInput
        $ input

------------------------------------------------------------------------
-- Time

-- | A 'Time' is a 'Date' with the possibility of having a specified
-- @HH:MM@ time.
data Time = Time
  { date :: Date
  , tod  :: Maybe TimeOfDay
  }
  deriving (Eq, Show)

-- | The time in HH:MM.
data TimeOfDay = TimeOfDay Int Int
  deriving (Eq)

instance Show TimeOfDay where
  show :: TimeOfDay -> String
  show (TimeOfDay h m) = pad h <> ":" <> pad m
   where
    pad :: Int -> String
    pad n = (if n <= 9 then "0" else "") <> show n

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

toOrgFmt :: Maybe TimeOfDay -> Day -> String
toOrgFmt tod day =
  mconcat ["<", isoDay, " ", take 3 $ show (dayOfWeek day), time, ">"]
 where
  time   :: String = maybe "" ((' ' :) . show) tod
  isoDay :: String = formatTime defaultTimeLocale (iso8601DateFormat Nothing) day

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
  = Scheduled String Time
  | Deadline  String Time
  | NormalMsg String
  deriving (Eq, Show)

-- | Pretty print a given 'Note'.
ppNote :: Clp -> String -> Note -> IO String
ppNote clp todo = \case
  Scheduled str time -> mkLine str "SCHEDULED: " (Just time)
  Deadline  str time -> mkLine str "DEADLINE: "  (Just time)
  NormalMsg str      -> mkLine str ""            Nothing
 where
  mkLine :: String -> String -> Maybe Time -> IO String
  mkLine str sched time = do
    t <- case time of
      Nothing -> pure ""
      Just ti -> (("\n  " <> sched) <>) <$> ppDate ti
    pure $ case clp of
      Body   c -> mconcat ["* ", todo, " ", str, t, c]
      Header c -> mconcat ["* ", todo, " [[", c, "][", str,"]]", t]

------------------------------------------------------------------------
-- Parsing

-- | Parse the given string into a 'Note'.
pInput :: String -> Maybe Note
pInput inp = (`runParser` inp) . choice $
  [ Scheduled <$> getLast "+s" <*> (Time <$> pDate <*> pTimeOfDay)
  , Deadline  <$> getLast "+d" <*> (Time <$> pDate <*> pTimeOfDay)
  , NormalMsg <$> munch1 (const True)
  ]
 where
  getLast :: String -> Parser String
  getLast ptn =  reverse
              .  dropWhile (== ' ')    -- trim whitespace at the end
              .  drop (length ptn)     -- drop only the last pattern
              .  reverse
              .  concat
             <$> endBy1 (go "") (pure ptn)
   where
    go :: String -> Parser String
    go consumed = do
      str  <- munch  (/= head ptn)
      word <- munch1 (/= ' ')
      bool go pure (word == ptn) $ consumed <> str <> word

-- | Try to parse a 'Time'.
pTimeOfDay :: Parser (Maybe TimeOfDay)
pTimeOfDay = choice
  [ Just <$> (TimeOfDay <$> pHour <* string ":" <*> pMinute) -- HH:MM
  , Just <$> (TimeOfDay <$> pHour               <*> pure 0 ) -- HH
  , pure Nothing
  ]
 where
  pMinute :: Parser Int = pNumBetween 1 60
  pHour   :: Parser Int = pNumBetween 1 24

-- | Parse a 'Date'.
pDate :: Parser Date
pDate = skipSpaces *> choice
  [ pPrefix "tod" "ay"    Today
  , pPrefix "tom" "orrow" Tomorrow
  , Next <$> pNext
  , Date <$> pDate'
  ] <* skipSpaces  -- cleanup
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
    (,,) <$> pNumBetween 1 31               -- day
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

  -- | Parse a prefix and drop a potential suffix up to the next (space
  -- separated) word.  If successful, return @ret@.
  pPrefix :: String -> String -> a -> Parser a
  pPrefix start leftover ret = do
    void $ string start
    l <- munch (/= ' ')
    guard (l `isPrefixOf` leftover)
    pure ret

-- | Parse a number between @lo@ (inclusive) and @hi@ (inclusive).
pNumBetween :: Int -> Int -> Parser Int
pNumBetween lo hi = do
  n <- num
  n <$ guard (n >= lo && n <= hi)
