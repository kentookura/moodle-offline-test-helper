module Example exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Parser
import Parsing exposing (..)
import Test exposing (..)


suite : Test
suite =
    describe "The Parser"
        [ test "works" <|
            \_ ->
                Expect.equal
                    (Ok [ [ True, False, True, False, False ] ])
                    (Parser.run quiz "xoxoo")
        ]
