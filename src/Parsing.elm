module Parsing exposing (..)

import Element exposing (column, text)
import Html
import Parser
    exposing
        ( (|.)
        , (|=)
        , DeadEnd
        , Parser
        , Step(..)
        , Trailing(..)
        , andThen
        , chompWhile
        , end
        , loop
        , oneOf
        , run
        , sequence
        , spaces
        , succeed
        , symbol
        )


exampleQuiz =
    """
TFTT
"""



--[ succeed (\b -> Loop (b :: bs))
--    |= answers
--
--, succeed () |> Parser.map (\_ -> Done (List.reverse bs))
--]
--
--


quiz : Parser (List (List Bool))
quiz =
    loop [] quizHelp


quizHelp : List (List Bool) -> Parser (Step (List (List Bool)) (List (List Bool)))
quizHelp ls =
    succeed
        (\l ->
            case l of
                [] ->
                    Done (List.reverse ls)

                _ ->
                    Loop (l :: ls)
        )
        |= line


line : Parser (List Bool)
line =
    loop [] lineHelp


lineHelp : List Bool -> Parser (Step (List Bool) (List Bool))
lineHelp bs =
    oneOf
        [ succeed (\b -> Loop (b :: bs))
            |. whitespaceButNotLinebreaks
            |= bool
            |. whitespaceButNotLinebreaks
        , eolOrEof
            |> Parser.map (\_ -> Done (List.reverse bs))
        ]


parse : String -> Result (List DeadEnd) (List Bool)
parse string =
    run answer string


main =
    case parse exampleQuiz of
        Err err ->
            Html.div []
                [ Html.div [] [ Html.text exampleQuiz ]
                , Html.text (Debug.toString err)
                ]

        Ok qz ->
            Html.div []
                [ Html.div [] [ Html.text exampleQuiz ]
                , Html.text (Debug.toString qz)
                ]


answer : Parser (List Bool)
answer =
    loop [] answerHelp


eolOrEof : Parser ()
eolOrEof =
    oneOf
        [ Parser.map (\_ -> ()) (symbol "\n")
        , Parser.map (\_ -> ()) end
        ]


whitespaceButNotLinebreaks : Parser ()
whitespaceButNotLinebreaks =
    chompWhile (\c -> c == ' ' || c == '\t')


answerHelp : List Bool -> Parser (Step (List Bool) (List Bool))
answerHelp bs =
    oneOf
        [ succeed (\b -> Loop (b :: bs))
            |= bool
            |. spaces
        , succeed ()
            |> Parser.map (\_ -> Done (List.reverse bs))
        ]


bool : Parser Bool
bool =
    oneOf
        [ Parser.map (\_ -> True) (symbol "X")
        , Parser.map (\_ -> False) (symbol "O")
        ]


boolToString : Bool -> String
boolToString b =
    if b then
        "True"

    else
        "False"


parseAnswers : String -> Result (List DeadEnd) (List Bool)
parseAnswers str =
    Parser.run answer str
