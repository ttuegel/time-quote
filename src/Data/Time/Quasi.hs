{-# LANGUAGE TypeApplications #-}

module Data.Time.Quasi where

import Data.Time (Day, LocalTime, TimeOfDay, TimeZone, UTCTime, ZonedTime)
import qualified Data.Time.Format.ISO8601 as Time
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote (QuasiQuoter(..))


utcTime :: QuasiQuoter
utcTime =
  QuasiQuoter
  { quoteExp = \str -> Time.iso8601ParseM @Q @UTCTime str >>= liftData
  , quotePat = \_ -> fail "utcTime: cannot quote pattern!"
  , quoteType = \_ -> fail "utcTime: cannot quote type!"
  , quoteDec = \_ -> fail "utcTime: cannot quote declaration!"
  }

day :: QuasiQuoter
day =
  QuasiQuoter
  { quoteExp = \str -> Time.iso8601ParseM @Q @Day str >>= liftData
  , quotePat = \_ -> fail "day: cannot quote pattern!"
  , quoteType = \_ -> fail "day: cannot quote type!"
  , quoteDec = \_ -> fail "day: cannot quote declaration!"
  }

timeOfDay :: QuasiQuoter
timeOfDay =
  QuasiQuoter
  { quoteExp = \str -> Time.iso8601ParseM @Q @TimeOfDay str >>= liftData
  , quotePat = \_ -> fail "timeOfDay: cannot quote pattern!"
  , quoteType = \_ -> fail "timeOfDay: cannot quote type!"
  , quoteDec = \_ -> fail "timeOfDay: cannot quote declaration!"
  }

localTime :: QuasiQuoter
localTime =
  QuasiQuoter
  { quoteExp = \str -> Time.iso8601ParseM @Q @LocalTime str >>= liftData
  , quotePat = \_ -> fail "localTime: cannot quote pattern!"
  , quoteType = \_ -> fail "localTime: cannot quote type!"
  , quoteDec = \_ -> fail "localTime: cannot quote declaration!"
  }

timeZone :: QuasiQuoter
timeZone =
  QuasiQuoter
  { quoteExp = \str -> Time.iso8601ParseM @Q @TimeZone str >>= liftData
  , quotePat = \_ -> fail "timeZone: cannot quote pattern!"
  , quoteType = \_ -> fail "timeZone: cannot quote type!"
  , quoteDec = \_ -> fail "timeZone: cannot quote declaration!"
  }

zonedTime :: QuasiQuoter
zonedTime =
  QuasiQuoter
  { quoteExp = \str -> Time.iso8601ParseM @Q @ZonedTime str >>= liftData
  , quotePat = \_ -> fail "zonedTime: cannot quote pattern!"
  , quoteType = \_ -> fail "zonedTime: cannot quote type!"
  , quoteDec = \_ -> fail "zonedTime: cannot quote declaration!"
  }
