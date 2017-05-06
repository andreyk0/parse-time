# parse-time

Parses time using a list of supported formats.

E.g.

```
import Data.Time.Parse
>
> parseUTCTime "12:01"
Just 2017-05-05 16:01:00 UTC
it :: Maybe UTCTime
>
> parseUTCTime "2017-05-05T16:01:00EST"
Just 2017-05-05 21:01:00 UTC
it :: Maybe UTCTime
> parseUTCTime "2017-05-05T16:01EST"
Just 2017-05-05 21:01:00 UTC
it :: Maybe UTCTime
```

See [source](src/Data/Time/Parse.hs) for a full list of formats.
