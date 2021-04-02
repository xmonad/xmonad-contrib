{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE StrictData          #-}
{-# LANGUAGE ScopedTypeVariables #-}
--------------------------------------------------------------------
-- |
-- Module      :  XMonad.Prompt.OrgMode
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
) where

import XMonad (X, io)
import XMonad.Prompt (XPConfig, XPrompt (showXPrompt), mkXPrompt)
import XMonad.Util.XSelection (getSelection)

import Control.Applicative (empty)
import Control.Monad ((<=<))
import Data.Char (isDigit)
import Data.Functor ((<&>))
import Data.List (foldl')
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Time (Day (ModifiedJulianDay), NominalDiffTime, UTCTime (utctDay), addUTCTime, defaultTimeLocale, formatTime, fromGregorian, getCurrentTime, iso8601DateFormat, nominalDay, toGregorian)
import System.Directory (getHomeDirectory)
import System.IO (IOMode (AppendMode), hPutStrLn, withFile)
import Text.ParserCombinators.ReadP (ReadP, munch, munch1, readP_to_S, skipSpaces, string, (<++))

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

There is also some scheduling and deadline functionality present.  They
are initiated by entering @+s@ or @+d@ into the prompt respectively,
followed by a date and a time of day.  Any of the following are valid
dates:

  - today
  - tomorrow
  - /any weekday/
  - /any date of the form DD MM YYYY/

In the last case, the month and the year are optional and will be, if
missing, filled out with the current month and year.  We disambiguate as
early as possible, so a simple @w@ will suffice to mean Wednesday, while
@s@ will not be enough to say Sunday.  Weekdays also always schedule
into the future, e.g. if today is Monday and you schedule something for
Monday, you will actually schedule it for the /next/ Monday (the one in
seven days).

The time is specified in the @HH:MM@ format.  The minutes may be
omitted, in which case @00@ will be substituted.

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

-- | Like 'orgPrompt', but fill in the primary selection as the contents
-- of the note.  The prompt will display a little @+ PS@ in the window
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
      NoClpSupport     -> pure ""
      PrimarySelection -> ("\n " <>) <$> getSelection

    -- Expand relative path with $HOME
    fp <- case orgFile of
      '/' : _ -> pure orgFile
      _       -> getHomeDirectory <&> (<> ('/' : orgFile))

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

-- | The time in HH:MM.
data TimeOfDay = TimeOfDay Int Int

instance Show TimeOfDay where
  show :: TimeOfDay -> String
  show (TimeOfDay h m) = show h <> ":" <> show m <> if m <= 9 then "0" else ""

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
  deriving (Show, Eq)

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

-- | Pretty print a given 'Note'.
ppNote :: String -> String -> Note -> IO String
ppNote clp todo = \case
  Scheduled str time -> mkLine str "SCHEDULED: " time
  Deadline  str time -> mkLine str "DEADLINE: "  time
  NormalMsg str      -> pure . mconcat $ ["* ", todo, " ", str, clp]
 where
  mkLine :: String -> String -> Time -> IO String
  mkLine inp sched
    = fmap (\d -> mconcat ["* ", todo, " ", inp, "\n  ", sched, d, clp])
    . ppDate

------------------------------------------------------------------------
-- Parsing

-- | Parse the given string into a 'Note'.
pInput :: String -> Maybe Note
pInput inp = fmap fst . listToMaybe . (`readP_to_S` inp) . lchoice $
  [ Scheduled <$> getLast "+s" <*> (Time <$> pDate <*> pTimeOfDay)
  , Deadline  <$> getLast "+d" <*> (Time <$> pDate <*> pTimeOfDay)
  , NormalMsg <$> munch1 (const True)
  ]
 where
  getLast :: String -> ReadP String
  getLast ptn = go ""
   where
    go :: String -> ReadP String = \consumed -> do
      next  <- munch1 (/= head ptn)
      next' <- munch1 (/= ' ')
      if next' == ptn
        then pure $ consumed <> next
        else go   $ consumed <> next <> next'

-- | Try to parse a 'Time'.
pTimeOfDay :: ReadP (Maybe TimeOfDay)
pTimeOfDay = lchoice
  [ Just <$> (TimeOfDay <$> pInt <* string ":" <*> pInt  ) -- HH:MM
  , Just <$> (TimeOfDay <$> pInt               <*> pure 0) -- HH
  , pure Nothing
  ]

-- | Parse a 'Date'.
pDate :: ReadP Date
pDate = skipSpaces *> lchoice
  [ Today    <$  string "tod"
  , Tomorrow <$  string "tom"
  , Next     <$> pNext
  , Date     <$> pDate1 <++ pDate2 <++ pDate3
  ] <* munch (/= ' ') <* skipSpaces  -- cleanup
 where
  pNext :: ReadP DayOfWeek = lchoice
    [ Monday   <$ string "m" , Tuesday <$ string "tu", Wednesday <$ string "w"
    , Thursday <$ string "th", Friday  <$ string "f" , Saturday  <$ string "sa"
    , Sunday   <$ string "su"
    ]

  -- XXX: This is really horrible, but I can't see a way to not have
  -- exponential blowup with ReadP otherwise.
  pDate1, pDate2, pDate3 :: ReadP (Int, Maybe Int, Maybe Integer)
  pDate1 = pDate' (fmap Just)            (fmap Just)
  pDate2 = pDate' (fmap Just)            (const (pure Nothing))
  pDate3 = pDate' (const (pure Nothing)) (const (pure Nothing))
  pDate'
    :: (ReadP Int     -> ReadP (f Int    ))
    -> (ReadP Integer -> ReadP (f Integer))
    -> ReadP (Int, f Int, f Integer)
  pDate' p p' =
    (,,) <$> pInt
         <*> p (skipSpaces *> lchoice
               [ 1  <$ string "ja" , 2  <$ string "f"  , 3  <$ string "mar"
               , 4  <$ string "ap" , 5  <$ string "may", 6  <$ string "jun"
               , 7  <$ string "jul", 8  <$ string "au" , 9  <$ string "s"
               , 10 <$ string "o"  , 11 <$ string "n"  , 12 <$ string "d"
               ])
         <*> p' (skipSpaces *> pInt)

-- | Parse a number.
pInt :: (Read a, Integral a) => ReadP a
pInt = read <$> munch1 isDigit

-- | Like 'choice', but with '(<++)' instead of '(+++)', stopping
-- parsing when the left-most parser succeeds.
lchoice :: [ReadP a] -> ReadP a
lchoice = foldl' (<++) empty

------------------------------------------------------------------------
-- Util

-- | Multivariant composition.
(.:) :: (a -> b) -> (c -> d -> a) -> c -> d -> b
(.:) = (.) . (.)
