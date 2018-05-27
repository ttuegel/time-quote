{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

{-
    time-quote: quasi-quotes for dates and times
    Copyright (C) 2018  Thomas Tuegel <ttuegel@mailbox.org>

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>.
 -}

{-|
    Module      : Data.Time.Quote
    Description : Quasi-quotes for dates and times
    Copyright   : (C) 2018  Thomas Tuegel <ttuegel@mailbox.org>
    License     : GPL-3
    Maintainer  : Thomas Tuegel <ttuegel@mailbox.org>

This module provides quasi-quoters to parse dates and times according to ISO
8601 formats.  By using quasi-quotes for literals, parse errors are found by the
compiler.

To use quasi-quotes, place the @LANGUAGE QuasiQuotes@ pragma at the top of the
source file or use the GHC command-line option @-XQuasiQuotes@. If using GHCi,
enter at the prompt

>>> :set -XQuasiQuotes

Quasi-quoters are provided for each type that defines an 'ISO8601' instance.
Use the quasi-quoters to quote a literal date or time of the corresponding type;
for example

> [utcTime| 2018-05-25T15:36:27.416462897Z |]  -- :: UTCTime

All quasi-quoters accept optional whitespace before and after the quoted
value. Refer to the list below for details about the expected format for each
type.
 -}

module Data.Time.Quote
    ( utcTime
    , day
    , timeOfDay
    , localTime
    , timeZone
    , zonedTime
    , calendarDiffDays
    , calendarDiffTime
    ) where

import qualified Data.Char as Char
import qualified Data.List as List
import Data.Time
    ( CalendarDiffDays(..), CalendarDiffTime(..), Day, LocalTime, TimeOfDay
    , TimeZone, UTCTime, ZonedTime )
import Data.Time.Format.ISO8601 (ISO8601)
import qualified Data.Time.Format.ISO8601 as Time
import Language.Haskell.TH.Lib
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote (QuasiQuoter(..))


-- $setup
-- >>> :set -XQuasiQuotes


-- | Quote a value of type 'UTCTime' in @yyyy-mm-ddThh:mm:ss[.sss]Z@ format.
--
-- >>> [utcTime| 2018-05-25T15:36:27.416462897Z |]
-- 2018-05-25 15:36:27.416462897 UTC
--
utcTime :: QuasiQuoter
utcTime =
  QuasiQuoter
  { quoteExp = \str -> parse @UTCTime str >>= liftData
  , quotePat = \_ -> fail "utcTime: cannot quote pattern!"
  , quoteType = \_ -> fail "utcTime: cannot quote type!"
  , quoteDec = \_ -> fail "utcTime: cannot quote declaration!"
  }

-- | Quote a value of type 'Day' in @yyyy-mm-dd@ format.
--
-- >>> [day| 2018-05-25 |]
-- 2018-05-25
--
day :: QuasiQuoter
day =
  QuasiQuoter
  { quoteExp = \str -> parse @Day str >>= liftData
  , quotePat = \_ -> fail "day: cannot quote pattern!"
  , quoteType = \_ -> fail "day: cannot quote type!"
  , quoteDec = \_ -> fail "day: cannot quote declaration!"
  }

-- | Quote a value of type 'TimeOfDay' in @hh:mm:ss[.sss]@ format.
--
-- >>> [timeOfDay| 15:36:27.416462897 |]
-- 15:36:27.416462897
--
timeOfDay :: QuasiQuoter
timeOfDay =
  QuasiQuoter
  { quoteExp = \str -> parse @TimeOfDay str >>= liftData
  , quotePat = \_ -> fail "timeOfDay: cannot quote pattern!"
  , quoteType = \_ -> fail "timeOfDay: cannot quote type!"
  , quoteDec = \_ -> fail "timeOfDay: cannot quote declaration!"
  }

-- | Quote a value of type 'LocalTime' in @yyyy-mm-ddThh:mm:ss[.sss]@ format.
--
-- >>> [localTime| 2018-05-25T10:36:27.416462897 |]
-- 2018-05-25 10:36:27.416462897
--
localTime :: QuasiQuoter
localTime =
  QuasiQuoter
  { quoteExp = \str -> parse @LocalTime str >>= liftData
  , quotePat = \_ -> fail "localTime: cannot quote pattern!"
  , quoteType = \_ -> fail "localTime: cannot quote type!"
  , quoteDec = \_ -> fail "localTime: cannot quote declaration!"
  }

-- | Quote a value of type 'TimeZone' in @±hh:mm@ format.
--
-- >>> [timeZone| -05:00 |]
-- -0500
--
timeZone :: QuasiQuoter
timeZone =
  QuasiQuoter
  { quoteExp = \str -> parse @TimeZone str >>= liftData
  , quotePat = \_ -> fail "timeZone: cannot quote pattern!"
  , quoteType = \_ -> fail "timeZone: cannot quote type!"
  , quoteDec = \_ -> fail "timeZone: cannot quote declaration!"
  }

-- | Quote a value of type 'ZonedTime' in @yyyy-mm-ddThh:mm:ss[.sss]±hh:mm@
-- format.
--
-- >>> [zonedTime| 2018-05-25T10:36:27.416462897-05:00 |]
-- 2018-05-25 10:36:27.416462897 -0500
--
zonedTime :: QuasiQuoter
zonedTime =
  QuasiQuoter
  { quoteExp = \str -> parse @ZonedTime str >>= liftData
  , quotePat = \_ -> fail "zonedTime: cannot quote pattern!"
  , quoteType = \_ -> fail "zonedTime: cannot quote type!"
  , quoteDec = \_ -> fail "zonedTime: cannot quote declaration!"
  }

-- | Quote a value of type 'CalendarDiffDays' in @PyYmMdD@ format.
--
-- >>> [calendarDiffDays| P2018Y5M25D |]
-- P24221M25D
--
calendarDiffDays :: QuasiQuoter
calendarDiffDays =
  QuasiQuoter
  { quoteExp = \str -> parse @CalendarDiffDays str >>= lift
  , quotePat = \_ -> fail "calendarDiffDays: cannot quote pattern!"
  , quoteType = \_ -> fail "calendarDiffDays: cannot quote type!"
  , quoteDec = \_ -> fail "calendarDiffDays: cannot quote declaration!"
  }
  where
    lift CalendarDiffDays {..} =
      [|
        CalendarDiffDays
        { cdMonths = $(liftData cdMonths)
        , cdDays = $(liftData cdDays)
        }
       |]

-- | Quote a value of type 'CalendarDiffTime' in @PyYmMdDThHmMs[.sss]S@ format.
--
-- >>> [calendarDiffTime| P2018Y5M25DT15H36M27.416462897S |]
-- P24221MT2216187.416462897S
--
calendarDiffTime :: QuasiQuoter
calendarDiffTime =
  QuasiQuoter
  { quoteExp = \str -> parse @CalendarDiffTime str >>= lift
  , quotePat = \_ -> fail "calendarDiffTime: cannot quote pattern!"
  , quoteType = \_ -> fail "calendarDiffTime: cannot quote type!"
  , quoteDec = \_ -> fail "calendarDiffTime: cannot quote declaration!"
  }
  where
    lift CalendarDiffTime {..} =
      [|
        CalendarDiffTime
        { ctMonths = $(liftData ctMonths)
        , ctTime = $(liftData ctTime)
        }
       |]


parse :: ISO8601 t => String -> Q t
parse = Time.iso8601ParseM . strip
  where
    strip = List.dropWhileEnd Char.isSpace . List.dropWhile Char.isSpace
