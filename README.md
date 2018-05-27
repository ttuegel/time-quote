# time-quote: quasi-quotes for time

[![Build Status](https://travis-ci.org/ttuegel/time-quote.svg?branch=master)](https://travis-ci.org/ttuegel/time-quote)

`time-quote` provides quasi-quoters to parse dates and times according to ISO 8601 formats.
By using quasi-quotes for literals, parse errors are found by the compiler.


## Usage

To use quasi-quotes, place the pragma at the top of the source file,

```.haskell
{-# LANGUAGE QuasiQuotes #-}
```

If using GHCi, enter at the prompt

```
> :set -XQuasiQuotes
```

`time-quote` provides quasi-quoters for each type from the `time` library that defines an `ISO8601` instance.
Use the quasi-quoters to quote a literal date or time of the corresponding type; for example

```.haskell
[utcTime| 2018-05-25T15:36:27.416462897Z |]  -- :: UTCTime
```

Refer to the API documentation for a complete list of the available quasi-quoters and details of the formats accepted.
