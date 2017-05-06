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
>
> parseUTCTime "2017-05-05T16:01EST"
Just 2017-05-05 21:01:00 UTC
it :: Maybe UTCTime
>
> parseUTCTime "-1d"
Just 2017-05-05 04:08:46.300676 UTC
it :: Maybe UTCTime
>
> parseUTCTime "-1h"
Just 2017-05-06 03:08:49.708381 UTC
it :: Maybe UTCTime
>
> parseUTCTime "-1m"
Just 2017-05-06 04:07:51.973447 UTC
it :: Maybe UTCTime
```

See [source](src/Data/Time/Parse.hs) for a full list of formats.
