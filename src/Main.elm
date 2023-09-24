port module Main exposing (main)

{-| A /very/ simple blog post with a custom inline element for some cool text formatting.

This is to get you started.

-}

import Browser
import Editor exposing (editor)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font exposing (bold, family, underline)
import Element.Input exposing (button)
import Html
import Html.Attributes as Attr
import Html.Events exposing (on)
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (requiredAt)
import Mark
import Mark.Error
import Markup exposing (toMoodleDefault, viewAnswerkey)
import Parser exposing (Trailing(..), deadEndsToString)
import Parsing exposing (quiz)


main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }


type alias Model =
    { mode : Mode
    , content :
        { questions : List (List Bool)
        , errors : List Mark.Error.Error
        }
    , source : String
    , helpVisible : Bool
    }


type Mode
    = Structured
    | Text
    | Undecided


init () =
    ( { mode = Text
      , helpVisible = False
      , source = ""
      , content =
            { questions = []
            , errors = []
            }
      }
    , Cmd.none
    )



--, Http.get
--    { url = "/exams/example.emu"
--    , expect = Http.expectString Api
--    }
--)


type Msg
    = Select Mode
    | SrcChanged String
    | Api (Result Http.Error String)
    | Copy
    | ToggleHelp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        { helpVisible, content } =
            model
    in
    case msg of
        Select m ->
            ( { model | mode = m }, Cmd.none )

        SrcChanged src ->
            case
                Parser.run quiz src
            of
                Ok qz ->
                    ( { model | source = src, content = { content | questions = qz } }, Cmd.none )

                Err err ->
                    ( { model | source = src }, Cmd.none )

        --( { model | content = { content | errors = List.append content.errors err } }, Cmd.none )
        --in
        --( { model | source = src }, Cmd.none )
        Api result ->
            case result of
                Ok src ->
                    ( { model | source = src }, Cmd.none )

                --( { model | content = compile quiz src content }, notifyEditor src )
                Err err ->
                    ( model, Cmd.none )

        ToggleHelp ->
            ( { model | helpVisible = not helpVisible }, Cmd.none )

        Copy ->
            ( model
            , content.questions
                |> toMoodleDefault
                |> copy
            )



--compile : Mark.Document (List (List Bool)) -> String -> {} -> {questions : List (List Bool), errors : List (Result _ _)}


compile quiz src content =
    case Mark.compile quiz src of
        Mark.Success qs ->
            { content | questions = qs }

        Mark.Almost { result, errors } ->
            -- This is the case where there has been an error,
            -- but it has been caught by `Mark.onError` and is still rendereable.
            { content | questions = result, errors = errors }

        Mark.Failure errors ->
            { content | errors = errors }


viewMoodle : String -> Element msg
viewMoodle src =
    el [ Font.family [ Font.monospace ] ] (text src)


viewErrors errors =
    List.map
        (html << Mark.Error.toHtml Mark.Error.Light)
        errors


view : Model -> Browser.Document Msg
view { mode, helpVisible, content, source } =
    case mode of
        Undecided ->
            { title = ""
            , body =
                [ layout []
                    (column [ centerX ]
                        [ textColumn []
                            [ paragraph []
                                [ text "This is a tool to simplify the use of the "
                                , newTabLink []
                                    { label = el [ underline ] (text "AMC's Moodle Offline Test Plugin")
                                    , url = "https://academic-moodle-cooperation.org/mod_offlinequiz/"
                                    }
                                , text ". It provides a markup format for creating quizzes in a more simple manner than the current implementation in Moodle."
                                ]
                            , column [ centerX ]
                                [ el [ Font.bold, centerX ] (text "Select Mode")
                                , row []
                                    (let
                                        card label msg matter =
                                            el [ alignTop, width fill ]
                                                (column []
                                                    [ button [ centerX, Background.color (rgb255 0 178 227), Border.rounded 8 ] { label = el [ padding 8 ] (text label), onPress = Just msg }
                                                    , paragraph [] [ text matter ]
                                                    ]
                                                )
                                     in
                                     [ card "Structured" (Select Structured) "Use this if you will provide the question sheet yourself (via LaTeX or something else)"
                                     , card "Text-based" (Select Text) "Use this if you want to use the question sheet generated by Moodle"

                                     --myButton [] "Simplified" (Select Simple)
                                     --myButton [] "Full Text" (Select Full)
                                     ]
                                    )
                                ]
                            ]
                        ]
                    )
                ]
            }

        _ ->
            let
                sidebarItem { label, msg } =
                    button [] { onPress = Just msg, label = label }

                toolbarItem { label, msg } =
                    button [] { onPress = Just msg, label = label }

                items =
                    [ { label = text "help", msg = ToggleHelp } ]

                toolbar =
                    List.map toolbarItem items
            in
            { title = "Edit Questions"
            , body =
                [ layout
                    (if helpVisible then
                        [ inFront
                            (column
                                [ centerX
                                , centerY
                                , Background.color (rgb 255 255 255)
                                ]
                                [ el [ bold ] (text "Example Quiz")
                                , text """
                                """
                                ]
                            )
                        ]

                     else
                        []
                    )
                  <|
                    column [ width fill, height fill ]
                        [ --row [ height (px 24) ] toolbar
                          row
                            [ alignTop, height fill, width fill ]
                            [ column [ width (px 48) ] []
                            , el [ height fill, width fill ]
                                (editor
                                    [ on "contentChanged" <|
                                        srcDecoder
                                    , Attr.attribute "src" source
                                    ]
                                )
                            , column [ width fill, height fill, alignTop ]
                                [ el [ height fill, width fill ]
                                    (content.questions
                                        |> viewAnswerkey
                                    )
<<<<<<< HEAD
                                , el [ height fill, width fill ]
                                    (paragraph []
                                        [ text "Use the editor to mark up your answer key. For Example \"XOXX\" corresponds to a question with 4 possible answers, with option 1, 3 and 4 are correct. Add more lines for more questions."
                                        ]
                                    )
=======
                                , column [ height fill, width fill ]
                                    [ paragraph []
                                        [ text "Use the editor to mark up your answer key. For example \"XOXX\" corresponds to a question with 4 possible answers, with option 1, 3 and 4 are correct. Add more lines for more questions."
                                        ]
                                    , button
                                        [ alignBottom
                                        , centerX
                                        , paddingEach { edges | bottom = 12 }
                                        ]
                                        { onPress = Just Copy
                                        , label =
                                            el
                                                [ Background.color (rgb 255 255 255)
                                                ]
                                                (text "Copy to Clipboard")
                                        }
                                    ]
>>>>>>> f46535b (make it usable)

                                --(case Parser.run quiz source of
                                --    Ok qz ->
                                --        qz |> toMoodleDefault |> text
                                --    Err deadEnds ->
                                --        text (deadEndsToString deadEnds)
                                --)
                                ]

                            --, column [] (viewErrors content.errors)
                            ]
                        ]
                ]
            }



{-
   Simple ->
       let
           toolbarItem { label, msg } =
               button [] { onPress = Just msg, label = label }

           items =
               [ { label = text "help", msg = ToggleHelp } ]

           toolbar =
               List.map toolbarItem items
       in
       { title = "Edit Questions"
       , body =
           [ layout [] <|
               column [ width fill, height fill ] []
           ]
       }

   Full ->
       { title = "Edit Questions"
       , body =
           [ layout [] <|
               column [ width fill, height fill ]
                   [ row [ height (px 24) ] []
                   , row
                       [ explain Debug.todo, alignTop, height fill, width fill ]
                       [ editor
                           [ on "contentChanged" <|
                               srcDecoder
                           , Attr.attribute "src" content.source
                           ]
                       , paragraph
                           [ alignTop ]
                           [ viewQuiz content.questions ]

                       --, column [ alignTop ]
                       --    [ html <|
                       --        Html.button
                       --            [ onClick Copy ]
                       --            [ Html.text "copy" ]
                       --    , content.questions
                       --        |> exportToMoodle
                       --        |> viewMoodle
                       --    ]
                       ]
                   ]
           ]
       }
-}


srcDecoder : Decoder Msg
srcDecoder =
    Decode.succeed SrcChanged
        |> requiredAt [ "detail", "value" ] Decode.string


edges =
    { top = 0, left = 0, right = 0, bottom = 0 }



-- | Copy to Clipboard


port copy : String -> Cmd msg


port notifyEditor : String -> Cmd msg
