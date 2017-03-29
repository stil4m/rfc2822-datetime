module Tests exposing (..)

import Rfc2822DateTimeTests
import Test exposing (concat, Test)


all : Test
all =
    concat
        [ Rfc2822DateTimeTests.suite
        ]
