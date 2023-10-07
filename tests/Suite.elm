module Suite exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Parser
import Parsing exposing (quiz)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "The Parser"
        [ test "works" <|
            \_ ->
                Expect.equal
                    (Ok [ [ True, False, True, False, False ] ])
                    (Parser.run quiz "xoxoo")
        ]
