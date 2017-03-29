module Rfc2822DatetimeTests exposing (suite)

import Test exposing (..)
import Expect
import Rfc2822Datetime exposing (Datetime, Month(..), Zone(..), WeekDay(..))


parseTuples : List ( String, Datetime )
parseTuples =
    [ ( "6 Mar 17 21:22 UT"
      , { dayOfWeek = Nothing
        , date = { year = 2017, month = Mar, day = 6 }
        , time = { hour = 21, minute = 22, second = Nothing, zone = UT }
        }
      )
    , ( "6 Mar 17 21:22:23 UT"
      , { dayOfWeek = Nothing
        , date = { year = 2017, month = Mar, day = 6 }
        , time = { hour = 21, minute = 22, second = Just 23, zone = UT }
        }
      )
    , ( "6 Mar 2017 21:22:23 GMT"
      , { dayOfWeek = Nothing
        , date = { year = 2017, month = Mar, day = 6 }
        , time = { hour = 21, minute = 22, second = Just 23, zone = GMT }
        }
      )
    , ( "06 Mar 2017 21:22:23 Z"
      , { dayOfWeek = Nothing
        , date = { year = 2017, month = Mar, day = 6 }
        , time = { hour = 21, minute = 22, second = Just 23, zone = Military 'Z' }
        }
      )
    , ( "Mon 06 Mar 2017 21:22:23 z"
      , { dayOfWeek = Just Mon
        , date = { year = 2017, month = Mar, day = 6 }
        , time = { hour = 21, minute = 22, second = Just 23, zone = Military 'z' }
        }
      )
    , ( "Mon, 06 Mar 2017 21:22:23 +0000"
      , { dayOfWeek = Just Mon
        , date = { year = 2017, month = Mar, day = 6 }
        , time = { hour = 21, minute = 22, second = Just 23, zone = Offset 0 }
        }
      )
    , ( "Thu, 13 Feb 1969 23:32:54 -0330"
      , { dayOfWeek = Just Thu
        , date = { year = 1969, month = Feb, day = 13 }
        , time = { hour = 23, minute = 32, second = Just 54, zone = Offset -330 }
        }
      )
    , ( "Thu,\x0D\n      13\x0D\n        Feb\x0D\n          1969\x0D\n      23:32\x0D\n               -0330"
      , { dayOfWeek = Just Thu
        , date = { year = 1969, month = Feb, day = 13 }
        , time = { hour = 23, minute = 32, second = Nothing, zone = Offset -330 }
        }
      )
    , ( "Thu,\n      13\n        Feb\n          1969\n      23:32\n               -0330"
      , { dayOfWeek = Just Thu
        , date = { year = 1969, month = Feb, day = 13 }
        , time = { hour = 23, minute = 32, second = Nothing, zone = Offset -330 }
        }
      )
    ]


testParseTuple : ( String, Datetime ) -> Test
testParseTuple ( input, output ) =
    test ("Should parse " ++ input) <|
        \() ->
            Rfc2822Datetime.parse input
                |> Expect.equal (Ok output)


suite : Test
suite =
    describe "Parser"
        [ describe "parse"
            (List.map testParseTuple parseTuples)
        ]
