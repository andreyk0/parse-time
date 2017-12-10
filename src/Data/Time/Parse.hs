{-# LANGUAGE OverloadedStrings #-}

{- | Attempts to parse time using one of the common time formats. | -}
module Data.Time.Parse
    ( allSupportedDateTimeFormats
    , formatIso8601DateTime
    , iso8601DateTimeFormat
    , parseIso8601DateTime
    , parseUTCTime
    , parseUTCTimeTzT
    ) where


import Control.Applicative
import Data.Maybe
import Data.Monoid
import Data.Time.Clock
import Data.Time.Format
import Data.Time.LocalTime


-- | Convenience shortcut, formats time using ISO8601 timestamp format.
formatIso8601DateTime :: (FormatTime t)
                      => t
                      -> String
formatIso8601DateTime = formatTime defaultTimeLocale iso8601DateTimeFormat


-- | Convenience shortcut, parses time using ISO8601 timestamp format.
parseIso8601DateTime :: (ParseTime t)
                     => String
                     -> Maybe t
parseIso8601DateTime =
  parseTimeM False defaultTimeLocale iso8601DateTimeFormat



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


-- | Local time formats, in the order of preference
localTimeFormats :: [String]
localTimeFormats = ["%H:%M:%S", "%H:%M"]


-- | Zoned datetime formats, in the order of preference
zonedIso8601DateTimeFormats :: [String]
zonedIso8601DateTimeFormats = (<> "%Z") <$> localIso8601DateTimeFormats


-- | All time formats this module can parse, e.g. to generate help messages.
allSupportedDateTimeFormats :: [String]
allSupportedDateTimeFormats =
  zonedIso8601DateTimeFormats
    <> localIso8601DateTimeFormats
        <> localTimeFormats
          <> [ "Nm", "Nh", "Nd" ] -- relative formats


-- | Parses a given string using all supported formats,
--   returns first match, if any.
parseUTCTime :: Bool -- ^ force relative time to always be in the past (e.g. '3h' would mean "3h ago")
             -> String -- ^ string to parse
             -> IO (Maybe UTCTime)
parseUTCTime forceRelPast s = do
  tz <- getCurrentTimeZone
  ts <- getCurrentTime
  return $ parseUTCTimeTzT forceRelPast tz ts s


-- | Parses a given string using all supported formats,
--   returns first match, if any.
parseUTCTimeTzT :: Bool -- ^ force relative time to always be in the past (e.g. '3h' would mean "3h ago")
                -> TimeZone -- ^ current time zone to assume when it's not a part of the input string
                -> UTCTime -- ^ current time to fill in date from when only H:M:S part is given
                -> String -- ^ string to parse
                -> Maybe UTCTime
parseUTCTimeTzT forceRelPast currentTz currentTs s =
  parseRelative
    <|> parseLocalTimeFormat
        <|> parseLocalDateTimeFormat
            <|> parseZonedDateTimeFormat

  where parseZonedDateTimeFormat = do t <- parseAny zonedIso8601DateTimeFormats
                                      return $ zonedTimeToUTC t

        parseLocalDateTimeFormat = do t <- parseAny localIso8601DateTimeFormats
                                      return $ zonedTimeToUTC $ t { zonedTimeZone = currentTz }

        currentDay = localDay $ utcToLocalTime currentTz currentTs

        parseLocalTimeFormat = do t <- parseAny localTimeFormats
                                  return $ zonedTimeToUTC $ ZonedTime (LocalTime currentDay (localTimeOfDay t)) currentTz

        parseAny fmts = listToMaybe $ catMaybes $ (\fmt -> parseTimeM False defaultTimeLocale fmt s) <$> fmts

        -- flips positive relative time to always mean "in the past"
        relPast n = fromIntegral $ if forceRelPast
                                   then negate $ abs n
                                   else n

        -- relative to current time stamp (days,hours,minutes)
        parseRelative = case reads s :: [(Int, String)]
                          of [(n,"d")] -> Just $ addUTCTime (relPast n * 24 * 60 * 60) currentTs
                             [(n,"h")] -> Just $ addUTCTime (relPast n *      60 * 60) currentTs
                             [(n,"m")] -> Just $ addUTCTime (relPast n *           60) currentTs
                             _ -> Nothing
