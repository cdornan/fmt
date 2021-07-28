{- Acknowledgements
~~~~~~~~~~~~~~~~~~~

This module has been largely copied off
    <https://hackage.haskell.org/package/formatting/docs/Formatting-Time.html>
Written by Chris Done
    <https://github.com/chrisdone>
-}

{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ExplicitForAll      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}


{- | Formatters for various time types. This module copies the structure of
@<https://hackage.haskell.org/package/formatting/docs/Formatting-Time.html Formatting.Time>@
from the @<https://hackage.haskell.org/package/formatting formatting>@ package.

Most of the time you'll want to use one of these formatters (all of the
examples below use @"2018-02-14 16:20:45.5 CST"@):

* 'dateTimeF' – full date and time:

    >>> dateTimeF t
    "Wed Feb 14 16:20:45 CST 2018"

* 'hmF' – hours and minutes:

    >>> hmF t
    "16:20"

* 'hmsF' – hours, minutes and seconds:

    >>> hmsF t
    "16:20:45"

* 'dateDashF' – date in ISO 8601 format:

    >>> dateDashF t
    "2018-02-14"

* 'diffF' – either a time period or a point in time, in a convenient for
  humans format:

    >>> diffF False 130    -- time period (130 seconds)
    "2 minutes"
    >>> diffF True 130     -- point in time (130 seconds in the future)
    "in 2 minutes"

Note that two formatters from @Formatting.Time@ are called differently here:

@
pico     -> 'picosecondF'
decimals -> 'subsecondF'
@

-}
module Fmt.Time
(
  -- * Custom
  timeF,

  -- * For 'TimeZone' (and 'ZonedTime' and 'UTCTime')
  tzF,
  tzNameF,
  dateTimeF,

  -- * For 'TimeOfDay' (and 'LocalTime' and 'ZonedTime' and 'UTCTime')
  hmF,
  hmsF,
  hmsLF,
  hmsPLF,
  dayHalfF,
  dayHalfUF,
  hour24F,
  hour12F,
  hour24SF,
  hour12SF,
  minuteF,
  secondF,
  picosecondF,
  subsecondF,

  -- * For 'UTCTime' and 'ZonedTime'
  epochF,

  -- * For 'Day' (and 'LocalTime' and 'ZonedTime' and 'UTCTime')
  dateSlashF,
  dateDashF,
  dateSlashLF,
  yearF,
  yyF,
  centuryF,
  monthNameF,
  monthNameShortF,
  monthF,
  dayOfMonthF,
  dayOfMonthOrdF,
  dayOfMonthSF,
  dayF,
  weekYearF,
  weekYYF,
  weekCenturyF,
  weekF,
  dayOfWeekF,
  dayNameShortF,
  dayNameF,
  weekFromZeroF,
  dayOfWeekFromZeroF,
  weekOfYearMonF,

  -- * Time spans, diffs, 'NominalDiffTime', 'DiffTime', etc.
  diffF,
  yearsF,
  daysF,
  hoursF,
  minutesF,
  secondsF,
)
where


import           Data.List               (find)

#if !MIN_VERSION_base(4,9,0)
import           Data.Monoid             ((<>))
#endif

import           Data.Text               (Text)
import qualified Data.Text               as T
import           Formatting.Buildable    (build)
import           Data.Text.Lazy.Builder  (Builder)
import           Data.Time

#if !MIN_VERSION_time(1,5,0)
import           Data.Time.Locale.Compat
#endif

import           Fmt.Internal.Numeric    (fixedF, ordinalF)

-- $setup
-- >>> let t = read "2018-02-14 16:20:45.5 CST" :: ZonedTime

----------------------------------------------------------------------------
-- Custom
----------------------------------------------------------------------------

-- | Format time with an arbitrary formatting string. Other formatters in
-- this module are implemented using 'timeF'.
timeF :: FormatTime a => Text -> a -> Builder
timeF f = build . T.pack . formatTime defaultTimeLocale (T.unpack f)

----------------------------------------------------------------------------
-- For 'TimeZone' (and 'ZonedTime' and 'UTCTime')
----------------------------------------------------------------------------

-- | Timezone offset on the format @-HHMM@.
--
-- >>> t
-- 2018-02-14 16:20:45.5 CST
-- >>> tzF t
-- "-0600"
tzF :: FormatTime a => a -> Builder
tzF = timeF "%z"

-- | Timezone name.
--
-- >>> tzNameF t
-- "CST"
tzNameF :: FormatTime a => a -> Builder
tzNameF = timeF "%Z"

-- | As 'dateTimeFmt' @locale@ (e.g. @%a %b %e %H:%M:%S %Z %Y@).
--
-- >>> dateTimeF t
-- "Wed Feb 14 16:20:45 CST 2018"
dateTimeF :: FormatTime a => a -> Builder
dateTimeF = timeF "%c"

----------------------------------------------------------------------------
-- For 'TimeOfDay' (and 'LocalTime' and 'ZonedTime' and 'UTCTime')
----------------------------------------------------------------------------

-- | Same as @%H:%M@.
--
-- >>> hmF t
-- "16:20"
hmF :: FormatTime a => a -> Builder
hmF = timeF "%R"

-- | Same as @%H:%M:%S@.
--
-- >>> hmsF t
-- "16:20:45"
hmsF :: FormatTime a => a -> Builder
hmsF = timeF "%T"

-- | As 'timeFmt' @locale@ (e.g. @%H:%M:%S@).
--
-- >>> hmsLF t
-- "16:20:45"
hmsLF :: FormatTime a => a -> Builder
hmsLF = timeF "%X"

-- | As 'time12Fmt' @locale@ (e.g. @%I:%M:%S %p@).
--
-- >>> hmsPLF t
-- "04:20:45 PM"
hmsPLF :: FormatTime a => a -> Builder
hmsPLF = timeF "%r"

-- | Day half from ('amPm' @locale@), converted to lowercase, @am@, @pm@.
--
-- >>> dayHalfF t
-- "pm"
dayHalfF :: FormatTime a => a -> Builder
dayHalfF = timeF "%P"

-- | Day half from ('amPm' @locale@), @AM@, @PM@.
--
-- >>> dayHalfUF t
-- "PM"
dayHalfUF :: FormatTime a => a -> Builder
dayHalfUF = timeF "%p"

-- | Hour, 24-hour, leading 0 as needed, @00@ - @23@.
--
-- >>> hour24F t
-- "16"
-- >>> hour24F midnight
-- "00"
hour24F :: FormatTime a => a -> Builder
hour24F = timeF "%H"

-- | Hour, 12-hour, leading 0 as needed, @01@ - @12@.
--
-- >>> hour12F t
-- "04"
-- >>> hour12F midnight
-- "12"
hour12F :: FormatTime a => a -> Builder
hour12F = timeF "%I"

-- | Hour, 24-hour, leading space as needed, @ 0@ - @23@.
--
-- >>> hour24SF t
-- "16"
-- >>> hour24SF midnight
-- " 0"
hour24SF :: FormatTime a => a -> Builder
hour24SF = timeF "%k"

-- | Hour, 12-hour, leading space as needed, @ 1@ - @12@.
--
-- >>> hour12SF t
-- " 4"
-- >>> hour12SF midnight
-- "12"
hour12SF :: FormatTime a => a -> Builder
hour12SF = timeF "%l"

-- | Minute, @00@ - @59@.
--
-- >>> minuteF t
-- "20"
minuteF :: FormatTime a => a -> Builder
minuteF = timeF "%M"

-- | Second, without decimal part, @00@ - @60@.
--
-- >>> secondF t
-- "45"
secondF :: FormatTime a => a -> Builder
secondF = timeF "%S"

-- | Picosecond, including trailing zeros, @000000000000@ - @999999999999@.
--
-- >>> picosecondF t
-- "500000000000"
picosecondF :: FormatTime a => a -> Builder
picosecondF = timeF "%q"

-- | Decimal point of the second. Up to 12 digits, without trailing zeros.
-- For a whole number of seconds, this produces an empty string.
--
-- >>> subsecondF t
-- ".5"
subsecondF :: FormatTime a => a -> Builder
subsecondF = timeF "%Q"

----------------------------------------------------------------------------
-- For 'UTCTime' and 'ZonedTime'
----------------------------------------------------------------------------

-- | Number of whole seconds since the Unix epoch. For times before the Unix
-- epoch, this is a negative number. Note that in @%s.%q@ and @%s%Q@ the
-- decimals are positive, not negative. For example, 0.9 seconds before the
-- Unix epoch is formatted as @-1.1@ with @%s%Q@.
--
-- >>> epochF t
-- "1518646845"
epochF :: FormatTime a => a -> Builder
epochF = timeF "%s"

----------------------------------------------------------------------------
-- For 'Day' (and 'LocalTime' and 'ZonedTime' and 'UTCTime')
----------------------------------------------------------------------------

-- | Same as @%m\/%d\/%y@.
--
-- >>> dateSlashF t
-- "02/14/18"
dateSlashF :: FormatTime a => a -> Builder
dateSlashF = timeF "%D"

-- | Same as @%Y-%m-%d@.
--
-- >>> dateDashF t
-- "2018-02-14"
dateDashF :: FormatTime a => a -> Builder
dateDashF = timeF "%F"

-- | As 'dateFmt' @locale@ (e.g. @%m\/%d\/%y@).
--
-- >>> dateSlashLF t
-- "02/14/18"
dateSlashLF :: FormatTime a => a -> Builder
dateSlashLF = timeF "%x"

-- | Year.
--
-- >>> yearF t
-- "2018"
yearF :: FormatTime a => a -> Builder
yearF = timeF "%Y"

-- | Last two digits of year, @00@ - @99@.
--
-- >>> yyF t
-- "18"
yyF :: FormatTime a => a -> Builder
yyF = timeF "%y"

-- | Century (being the first two digits of the year), @00@ - @99@.
--
-- >>> centuryF t
-- "20"
centuryF :: FormatTime a => a -> Builder
centuryF = timeF "%C"

-- | Month name, long form ('fst' from 'months' @locale@), @January@ -
-- @December@.
--
-- >>> monthNameF t
-- "February"
monthNameF :: FormatTime a => a -> Builder
monthNameF = timeF "%B"

-- | Month name, short form ('snd' from 'months' @locale@), @Jan@ - @Dec@.
--
-- >>> monthNameShortF t
-- "Feb"
monthNameShortF :: FormatTime a => a -> Builder
monthNameShortF = timeF "%b"

-- | Month of year, leading 0 as needed, @01@ - @12@.
--
-- >>> monthF t
-- "02"
monthF :: FormatTime a => a -> Builder
monthF = timeF "%m"

-- | Day of month, leading 0 as needed, @01@ - @31@.
--
-- >>> dayOfMonthF t
-- "14"
dayOfMonthF :: FormatTime a => a -> Builder
dayOfMonthF = timeF "%d"

-- | Day of month, @1st@, @2nd@, @25th@, etc.
--
-- >>> dayOfMonthOrdF t
-- "14th"
dayOfMonthOrdF :: FormatTime a => a -> Builder
dayOfMonthOrdF = ordinalF . timeToInt
  where
    timeToInt :: FormatTime a => a -> Int
    timeToInt = read . formatTime defaultTimeLocale "%d"

-- | Day of month, leading space as needed, @ 1@ - @31@.
dayOfMonthSF :: FormatTime a => a -> Builder
dayOfMonthSF = timeF "%e"

-- | Day of year for Ordinal Date format, @001@ - @366@.
--
-- >>> dayF t
-- "045"
dayF :: FormatTime a => a -> Builder
dayF = timeF "%j"

-- | Year for Week Date format e.g. @2013@.
--
-- >>> weekYearF t
-- "2018"
weekYearF :: FormatTime a => a -> Builder
weekYearF = timeF "%G"

-- | Last two digits of year for Week Date format, @00@ - @99@.
--
-- >>> weekYYF t
-- "18"
weekYYF :: FormatTime a => a -> Builder
weekYYF = timeF "%g"

-- | Century (first two digits of year) for Week Date format, @00@ - @99@.
--
-- >>> weekCenturyF t
-- "20"
weekCenturyF :: FormatTime a => a -> Builder
weekCenturyF = timeF "%f"

-- | Week for Week Date format, @01@ - @53@.
--
-- >>> weekF t
-- "07"
weekF :: FormatTime a => a -> Builder
weekF = timeF "%V"

-- | Day for Week Date format, @1@ - @7@.
--
-- >>> dayOfWeekF t
-- "3"
dayOfWeekF :: FormatTime a => a -> Builder
dayOfWeekF = timeF "%u"

-- | Day of week, short form ('snd' from 'wDays' @locale@), @Sun@ - @Sat@.
--
-- >>> dayNameShortF t
-- "Wed"
dayNameShortF :: FormatTime a => a -> Builder
dayNameShortF = timeF "%a"

-- | Day of week, long form ('fst' from 'wDays' @locale@), @Sunday@ -
-- @Saturday@.
--
-- >>> dayNameF t
-- "Wednesday"
dayNameF :: FormatTime a => a -> Builder
dayNameF = timeF "%A"

-- | Week number of year, where weeks start on Sunday (as
-- 'sundayStartWeek'), @00@ - @53@.
--
-- >>> weekFromZeroF t
-- "06"
weekFromZeroF :: FormatTime a => a -> Builder
weekFromZeroF = timeF "%U"

-- | Day of week number, @0@ (= Sunday) - @6@ (= Saturday).
--
-- >>> dayOfWeekFromZeroF t
-- "3"
dayOfWeekFromZeroF :: FormatTime a => a -> Builder
dayOfWeekFromZeroF = timeF "%w"

-- | Week number of year, where weeks start on Monday (as
-- 'mondayStartWeek'), @00@ - @53@.
--
-- >>> weekOfYearMonF t
-- "07"
weekOfYearMonF :: FormatTime a => a -> Builder
weekOfYearMonF = timeF "%W"

----------------------------------------------------------------------------
-- Time spans, diffs, 'NominalDiffTime', 'DiffTime', etc.
----------------------------------------------------------------------------

-- | Display a time span as one time relative to another. Input is assumed to
-- be seconds. Typical inputs are 'NominalDiffTime' and 'DiffTime'.
--
-- >>> diffF False 100
-- "a minute"
-- >>> diffF True 100
-- "in a minute"
diffF :: forall n . RealFrac n
      => Bool     -- ^ Whether to display the @in/ago@ prefix or not
      -> n        -- ^ Example: @3 seconds ago@, @in 2 days@
      -> Builder
diffF fix = diffed
  where
    diffed :: RealFrac n => n -> Builder
    diffed ts =
      case find (\(s,_,_) -> abs ts >= s) (reverse ranges) of
        Nothing           -> "unknown"
        Just (_, f, base) -> prefix <> f (toInt ts base) <> suffix
      where
        prefix = if fix && ts > 0 then "in "  else ""
        suffix = if fix && ts < 0 then " ago" else ""

    toInt :: RealFrac n => n -> n -> Int
    toInt ts base = abs (round (ts / base))

    intF :: Builder -> Int -> Builder
    intF t n = build n <> t

    ranges :: RealFrac n => [(n, Int -> Builder, n)]
    ranges =
      [ (0           , intF  " milliseconds" , 0.001 )
      , (1           , intF  " seconds"      , 1     )
      , (minute      , const "a minute"      , 0     )
      , (minute * 2  , intF  " minutes"      , minute)
      , (minute * 30 , const "half an hour"  , 0     )
      , (minute * 31 , intF  " minutes"      , minute)
      , (hour        , const "an hour"       , 0     )
      , (hour * 2    , intF  " hours"        , hour  )
      , (hour * 3    , const "a few hours"   , 0     )
      , (hour * 4    , intF  " hours"        , hour  )
      , (day         , const "a day"         , 0     )
      , (day * 2     , intF  " days"         , day   )
      , (week        , const "a week"        , 0     )
      , (week * 2    , intF  " weeks"        , week  )
      , (month       , const "a month"       , 0     )
      , (month * 2   , intF  " months"       , month )
      , (year        , const "a year"        , 0     )
      , (year * 2    , intF  " years"        , year  )
      ]
      where year   = month  * 12
            month  = day    * 30
            week   = day    * 7
            day    = hour   * 24
            hour   = minute * 60
            minute = 60

-- | Display the absolute value time span in years.
--
-- >>> epochF t    -- time passed since Jan 1, 1970
-- "1518646845"
-- >>> yearsF 3 1518646845
-- "48.156"
yearsF :: RealFrac n
       => Int -- ^ Decimal places.
       -> n
       -> Builder
yearsF n = fixedF n . abs . count
  where count x = x / 365 / 24 / 60 / 60

-- | Display the absolute value time span in days.
--
-- >>> daysF 3 1518646845
-- "17576.931"
daysF :: RealFrac n
      => Int -- ^ Decimal places.
      -> n
      -> Builder
daysF n = fixedF n . abs . count
  where count x = x / 24 / 60 / 60

-- | Display the absolute value time span in hours.
--
-- >>> hoursF 3 3600
-- "1.000"
hoursF :: RealFrac n
       => Int -- ^ Decimal places.
       -> n
       -> Builder
hoursF n = fixedF n . abs . count
  where count x = x / 60 / 60

-- | Display the absolute value time span in minutes.
--
-- >>> minutesF 3 150
-- "2.500"
minutesF :: RealFrac n
         => Int -- ^ Decimal places.
         -> n
         -> Builder
minutesF n = fixedF n . abs . count
  where count x = x / 60

-- | Display the absolute value time span in seconds.
--
-- >>> secondsF 3 100
-- "100.000"
secondsF :: RealFrac n
         => Int -- ^ Decimal places.
         -> n
         -> Builder
secondsF n = fixedF n . abs
