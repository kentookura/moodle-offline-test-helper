module Question exposing (..)

import Element exposing (..)
import Html
import Mark
import Mark.Error
import Parser exposing ((|=), DeadEnd, Parser, Step(..), Trailing(..), loop, oneOf, succeed, symbol)
import String exposing (fromFloat, fromInt, left)


type alias Question =
    { question : String
    , title : String
    , answers : List Bool
    }


exportToGift : List Question -> String
exportToGift =
    List.map toGift >> String.join "\n\n"


toGift : Question -> String
toGift q =
    String.join "\n"
        [ "::" ++ q.title
        , "::" ++ q.question ++ "{"
        , q.answers |> answerKey
        , "}"
        ]


answerKey : List Bool -> String
answerKey bools =
    let
        trues =
            bools
                |> List.filter (\x -> x == True)
                |> List.length
                |> toFloat

        percentPerCorrectAnswer =
            100.0 / trues

        points : Bool -> Float
        points b =
            if b then
                percentPerCorrectAnswer

            else
                negate percentPerCorrectAnswer

        template =
            \i b ->
                "~%" ++ (left 8 << fromFloat << points) b ++ "%" ++ "Answer " ++ fromInt i
    in
    bools
        |> List.indexedMap (\i b -> template i b)
        |> String.join "\n"


viewQuestion : Question -> Element msg
viewQuestion q =
    row []
        (List.map (\b -> text (boolToString b)) q.answers)



--()


viewExam : List Question -> Element msg
viewExam questions =
    column [] <|
        List.map
            (\q ->
                html <|
                    Html.div []
                        [ Html.h1 [] [ Html.text q.title ]
                        , Html.div [] [ Html.text q.question ]
                        , Html.div [] (List.map (\b -> Html.text (boolToString b)) q.answers)
                        ]
            )
            questions


exam : Mark.Document (List Question)
exam =
    Mark.document
        (\l -> l)
        (Mark.manyOf
            [ question
            ]
        )


question : Mark.Block Question
question =
    Mark.record "Question"
        (\q a title ->
            { question = q
            , answers = a
            , title = title
            }
        )
        |> Mark.field "question" Mark.string
        |> Mark.field "answers" answer
        |> Mark.field "title" Mark.string
        |> Mark.toBlock


answer : Mark.Block (List Bool)
answer =
    Mark.verify
        (\str ->
            str
                |> Parser.run answers
                |> Result.mapError
                    (\_ -> badAnswerMessage)
        )
        Mark.string


answers : Parser (List Bool)
answers =
    loop [] answerHelp


answerHelp : List Bool -> Parser (Step (List Bool) (List Bool))
answerHelp bs =
    oneOf
        [ succeed (\b -> Loop (b :: bs))
            |= bool
        , succeed ()
            |> Parser.map (\_ -> Done (List.reverse bs))
        ]


bool : Parser Bool
bool =
    oneOf
        [ Parser.map (\_ -> True) (symbol "T")
        , Parser.map (\_ -> False) (symbol "F")
        ]


boolToString : Bool -> String
boolToString b =
    if b then
        "True"

    else
        "False"


parseAnswers : String -> Result (List DeadEnd) (List Bool)
parseAnswers str =
    Parser.run answers str


badAnswerMessage : Mark.Error.Custom
badAnswerMessage =
    Mark.Error.Custom
        ""
        [ "I was trying to parse the answers to a question, but I failed" ]
