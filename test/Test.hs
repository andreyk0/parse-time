{-# LANGUAGE OverloadedStrings  #-}

module Main where

import Data.Time.LocalTime
import Data.Time.Parse
import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.HUnit


main = defaultMain tests

tests = [
    testGroup "Parse time" [
      testCase "parse zoned time" test_parseZonedTime
    , testCase "parse local time" test_parseLocalTime
    , testCase "parse hms" test_parseHMS
    , testCase "parse relative" test_parseRelative
    , testCase "parse relative force past" test_parseRelativeForcePast
    ]
  ]


test_parseZonedTime = do
  tryParseTime "2017-05-06T02:22:27UTC" "2017-05-06T02:22:27UTC"
  tryParseTime "2017-05-06T02:22:27EST" "2017-05-06T07:22:27UTC"
  tryParseTime "2017-05-06T02:22EST"    "2017-05-06T07:22:00UTC"
  tryParseTime "2017-05-06T02EST"       "2017-05-06T07:00:00UTC"
  tryParseTime "2017-05-06EST"          "2017-05-06T05:00:00UTC"
  tryParseTime "2017-05-06EST"          "2017-05-06T05:00:00UTC"
  tryParseTime "2017-05-06Z"            "2017-05-06T00:00:00UTC"


test_parseLocalTime = do
  tryParseTime "2017-05-05T22:53:24"    "2017-05-06T02:53:24UTC"
  tryParseTime "2017-05-05T22:53"       "2017-05-06T02:53:00UTC"
  tryParseTime "2017-05-05T22"          "2017-05-06T02:00:00UTC"
  tryParseTime "2017-05-05"             "2017-05-05T04:00:00UTC"


test_parseHMS = do
  tryParseTime "22:53:24"    "2017-05-06T02:53:24UTC"
  tryParseTime "22:53"       "2017-05-06T02:53:00UTC"


test_parseRelative = do
  tryParseTime "-10m"  "2017-05-06T02:54:05UTC"
  tryParseTime "3m"    "2017-05-06T03:07:05UTC"
  tryParseTime "-2h"   "2017-05-06T01:04:05UTC"
  tryParseTime "-3d"   "2017-05-03T03:04:05UTC"


test_parseRelativeForcePast = do
  tryParseTimeForceRelativePast True "-10m"  "2017-05-06T02:54:05UTC"
  tryParseTimeForceRelativePast True "3m"    "2017-05-06T03:01:05UTC"
  tryParseTimeForceRelativePast True "2h"    "2017-05-06T01:04:05UTC"
  tryParseTimeForceRelativePast True "3d"    "2017-05-03T03:04:05UTC"


tryParseTime :: String -> String -> IO ()
tryParseTime = tryParseTimeForceRelativePast False


tryParseTimeForceRelativePast :: Bool -> String -> String -> IO ()
tryParseTimeForceRelativePast forceRelPast arg expStr = do
  let tz = hoursToTimeZone (- 4)

  testNow <- case parseIso8601DateTime "2017-05-06T03:04:05UTC"
               of Just t -> return t
                  Nothing -> error expStr

  expTm <- case parseIso8601DateTime expStr
             of Just t -> return t
                Nothing -> error expStr

  parseUTCTimeTzT forceRelPast tz testNow arg @?= Just expTm
