{-# LANGUAGE OverloadedStrings #-}

{- | Attempts to parse time using one of the common time formats. | -}
module Data.Time.Parse
    ( allSupportedDateTimeFormats
    , formatIso8601DateTime
    , iso8601DateTimeFormat
    , parseUTCTime
    ) where


import Control.Applicative
import Data.Maybe
import Data.Monoid
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Time.Format
import Data.Time.LocalTime


-- | Convenience shortcut, formats time using ISO8601 timestamp format.
formatIso8601DateTime :: (FormatTime t)
                      => t
                      -> String
formatIso8601DateTime = formatTime defaultTimeLocale iso8601DateTimeFormat


-- | Zoned ISO8601 datetime
iso8601DateTimeFormat :: String
iso8601DateTimeFormat = iso8601DateFormat (Just "%H:%M:%S%Z")


-- | Local datetime formats, in the order of preference
localIso8601DateTimeFormats :: [String]
localIso8601DateTimeFormats =
  [ iso8601DateFormat (Just "%H:%M:%S")
  , iso8601DateFormat (Just "%H:%M")
  , iso8601DateFormat (Just "%H")
  , iso8601DateFormat Nothing
  ]


-- | Zoned datetime formats, in the order of preference
zonedIso8601DateTimeFormats :: [String]
zonedIso8601DateTimeFormats = (<> "%Z") <$> localIso8601DateTimeFormats


-- | All time formats this module can parse, e.g. to generate help messages.
allSupportedDateTimeFormats :: [String]
allSupportedDateTimeFormats =
  zonedIso8601DateTimeFormats
    <> localIso8601DateTimeFormats


-- | Parses a given string using all supported formats,
--   returns first match, if any.
parseUTCTime :: TimeZone -- ^ current time zone to assume when it's not a part of the input string
             -> String -- ^ string to parse
             -> Maybe UTCTime
parseUTCTime currentTz s =
  parseLocalDateTimeFormat -- need to parse zone-less formats first, %Z is tolerant of empty input
    <|> parseZonedDateTimeFormat

  where parseZonedDateTimeFormat = do t <- parseAny zonedIso8601DateTimeFormats
                                      return $ zonedTimeToUTC t

        parseLocalDateTimeFormat = do t <- parseAny localIso8601DateTimeFormats
                                      return $ zonedTimeToUTC $ t { zonedTimeZone = currentTz }

        parseAny fmts = listToMaybe $ catMaybes $ (\fmt -> parseTimeM False defaultTimeLocale fmt s) <$> fmts
