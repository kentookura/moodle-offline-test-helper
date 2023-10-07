module Parsing exposing (quiz)

import Parser
    exposing
        ( (|.)
        , (|=)
        , Parser
        , Step(..)
        , Trailing(..)
        , chompWhile
        , end
        , loop
        , oneOf
        , succeed
        , symbol
        )


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


eolOrEof : Parser ()
eolOrEof =
    oneOf
        [ Parser.map (\_ -> ()) (symbol "\n")
        , Parser.map (\_ -> ()) end
        ]


whitespaceButNotLinebreaks : Parser ()
whitespaceButNotLinebreaks =
    chompWhile (\c -> c == ' ' || c == '\t')


bool : Parser Bool
bool =
    oneOf
        [ Parser.map (\_ -> True) (symbol "x")
        , Parser.map (\_ -> False) (symbol "o")
        ]
