module Tests exposing (..)

import Rfc2822DatetimeTests
import Test exposing (concat, Test)


all : Test
all =
    concat
        [ Rfc2822DatetimeTests.suite
        ]
