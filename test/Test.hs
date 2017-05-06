{-# LANGUAGE OverloadedStrings  #-}

module Main where

import Data.Time.Format
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


tryParseTime :: String -> String -> IO ()
tryParseTime arg expStr = do
  let tz = hoursToTimeZone (- 4)

  expTm <- case parseTimeM False defaultTimeLocale iso8601DateTimeFormat expStr
             of Just t -> return t
                Nothing -> error expStr

  parseUTCTime tz arg @?= Just expTm
