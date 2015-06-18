import String

import IO.IO exposing (..)
import IO.Runner exposing (Request, Response, run)
import ElmTest.Runner.Console exposing (runDisplay)
import ElmTest.Test exposing (..)

import RogueTest

tests : Test
tests = suite "Rogue Tests"
        [ RogueTest.tests
        ]

port requests : Signal Request
port requests = run responses (runDisplay tests)

port responses : Signal Response
