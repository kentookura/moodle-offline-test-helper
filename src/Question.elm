module Question exposing (..)

import Browser
import Html exposing (Html, button, div)
import Html.Attributes as Attr
import Html.Events exposing (onClick)
import Html.String
import Http
import Mark
import Mark.Error
import Parser exposing ((|=), DeadEnd, Parser, Step(..), Trailing(..), keyword, loop, oneOf, spaces, succeed, symbol)
import Text exposing (text)


type alias Question =
    { question : String
    , title : String
    , answers : List Bool
    }


toGift : Question -> String
toGift q =
    String.join "\n"
        [ "::" ++ q.title
        , "::" ++ q.question ++ "{"
        , q.answers |> toGift_
        , "}"
        ]


toGift_ : List Bool -> String
toGift_ bs =
    let
        trues =
            (toFloat << List.length << List.filter (\x -> x == True)) bs

        percentPerCorrectAnswer =
            100.0 / trues

        points : Bool -> Float
        points b =
            if b then
                percentPerCorrectAnswer

            else
                negate percentPerCorrectAnswer
    in
    bs
        |> List.indexedMap
            (\i b ->
                "~%" ++ (String.left 8 << String.fromFloat << points) b ++ "%" ++ "Answer " ++ String.fromInt i
            )
        |> String.join "\n"


viewQuestion : Question -> Html msg
viewQuestion q =
    Html.div []
        [ Html.h1 [] [ Html.text q.title ]
        , Html.div [] [ Html.text q.question ]
        , Html.div [] (List.map (\b -> Html.text (boolToString b)) q.answers)
        ]



--viewAnswerkey bs = Html.div [] (List.map (\b -> Html.text (boolToString b)) bs)


exam : Mark.Document (List Question)
exam =
    Mark.document
        (\l -> l)
        -- We have some required metadata that starts our document.
        (Mark.manyOf
            [ question
            ]
        )



--exam : Mark.Block
--exam =  Mark.record "Question"
--  (\q a title ->
--      { question = q
--      , answers = a
--      , title = title
--      }
--  )
--  |> Mark.field "question" Mark.string
--  |> Mark.field "answers" answer
--  |> Mark.field "title" text
--  |> Mark.toBlock


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



--sequence
--   { start = ""
--   , separator = ""
--   , end = ""
--   , spaces = spaces
--   , item = oneOf
--      [ Parser.map (\_ -> True) (keyword "T")
--      , Parser.map (\_ -> False) (keyword "F")
--      ]
--   , trailing = Optional
--   }


answerHelp : List Bool -> Parser (Step (List Bool) (List Bool))
answerHelp bs =
    oneOf
        [ succeed (\b -> Loop (b :: bs))
            |= bool
        , succeed ()
            |> Parser.map (\_ -> Done (List.reverse bs))
        ]


bool =
    oneOf
        [ Parser.map (\_ -> True) (symbol "T")
        , Parser.map (\_ -> False) (symbol "F")
        ]


boolToString : Bool -> String
boolToString b =
    case b of
        False ->
            "False"

        True ->
            "True"


parseAnswers : String -> Result (List DeadEnd) (List Bool)
parseAnswers str =
    Parser.run answers str


badAnswerMessage =
    Mark.Error.Custom
        ""
        [ "I was trying to parse the answers to a question, but I failed" ]
