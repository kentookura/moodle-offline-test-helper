module Markup exposing (toMoodleDefault, viewAnswerkey)

import Element exposing (..)
import Element.Input as Input exposing (checkbox, labelHidden)
import Mark
import Mark.Error
import Parser
import Parsing exposing (quiz)
import String exposing (fromFloat, fromInt, left)



--import Text exposing (text)


type alias Moodle =
    String



--toMoodle : Question -> Moodle


toMoodle q =
    String.join "\n"
        [ "::" ++ q.title
        , "::" ++ q.question ++ "{"
        , q.answers |> answerKey
        , "}"
        ]


toMoodleDefault : List (List Bool) -> String
toMoodleDefault bs =
    let
        template i ans =
            String.join "\n"
                [ "::" ++ "Title " ++ fromInt i
                , "::" ++ "Question :" ++ "{"
                , ans |> answerKey
                , "}"
                ]
    in
    bs
        |> List.indexedMap (\i b -> template i b)
        --|> List.map toMoodle
        |> String.join "\n"


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



--exam : Mark.Document (Element msg)
--exam =
--    Mark.document
--        (\l -> l)
--        (Mark.manyOf
--            [ quizEl
--            , Mark.map (paragraph []) text
--            ]
--        )
--viewQuestion : Question -> Element msg
--viewQuestion q =
--    row []
--        (List.map (\b -> text (boolToString b)) q.answers)


viewAnswerkey : List (List Bool) -> Element msg
viewAnswerkey questions =
    let
        box b =
            el [ padding 4 ] (Input.defaultCheckbox b)
    in
    column []
        (List.indexedMap
            (\i qs ->
                column []
                    [ text ("Question " ++ fromInt i ++ ":")
                    , row [ padding 8 ]
                        (List.map
                            box
                            qs
                        )
                    ]
            )
            questions
        )



--[ el [] (text q.title)
--, el [] (text q.question)
--, row [] (List.map (\b -> text (boolToString b)) q.answers)
--]
--viewQuiz : List Question -> Element msg
--viewQuiz questions =
--    column [] <|
--        List.map
--            (\q ->
--                column []
--                    [ el [] (text q.title)
--                    , el [] (text q.question)
--                    , row [] (List.map (\b -> text (boolToString b)) q.answers)
--                    ]
--            )
--            questions
--quizList : Mark.Block (List (Element msg))
--quizList =
--    Mark.map
--        (List.map viewQuestion)
--        (Mark.manyOf [ question ])
--quizEl : Mark.Block (Element msg)
--quizEl =
--    Mark.map viewQuestion question
--quiz : Mark.Document (List Question)
--quiz =
--    Mark.document
--        (\l -> l)
--        (Mark.manyOf
--            [ question
--            ]
--        )
--question : Mark.Block Question


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


answer : Mark.Block (List (List Bool))
answer =
    Mark.verify
        (\str ->
            str
                |> Parser.run quiz
                |> Result.mapError
                    (\_ -> badAnswerMessage)
        )
        Mark.string


badAnswerMessage : Mark.Error.Custom
badAnswerMessage =
    Mark.Error.Custom
        ""
        [ "I was trying to parse the answers to a question, but I failed" ]
