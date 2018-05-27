{-# LANGUAGE QuasiQuotes #-}
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

module Data.Time.Quote
    ( utcTime
    , day
    , timeOfDay
    , localTime
    , timeZone
    , zonedTime
    ) where

import qualified Data.Char as Char
import qualified Data.List as List
import Data.Time (Day, LocalTime, TimeOfDay, TimeZone, UTCTime, ZonedTime)
import Data.Time.Format.ISO8601 (ISO8601)
import qualified Data.Time.Format.ISO8601 as Time
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote (QuasiQuoter(..))


-- $setup
-- >>> :set -XQuasiQuotes


-- |
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

-- |
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

-- |
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

-- |
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

-- |
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

-- |
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


parse :: ISO8601 t => String -> Q t
parse = Time.iso8601ParseM . strip
  where
    strip = List.dropWhileEnd Char.isSpace . List.dropWhile Char.isSpace
