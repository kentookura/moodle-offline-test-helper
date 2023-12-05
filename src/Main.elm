module Main exposing (main)

import Browser
import Color
import Element exposing (..)
import Element.Border as Border
import Element.Font as Font exposing (bold)
import Element.Input exposing (button, defaultCheckbox, labelHidden, multiline, placeholder)
import File.Download as Download
import Html
import Html.Attributes exposing (controls, src, type_)
import Http
import List exposing (length, range)
import List.Zipper exposing (Zipper, current, fromCons, isFirst, isLast, next, previous)
import Mark.Error
import Material.Icons as Filled
import Material.Icons.Types exposing (Coloring(..))
import Parser exposing (DeadEnd, Trailing(..))
import Parsing exposing (quiz)
import String exposing (fromFloat, fromInt, left)
import Task
import Time exposing (Month(..))
import VitePluginHelper as Vite


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }


type alias Model =
    { content :
        { questions : List (List Bool)
        , errors : List Mark.Error.Error
        }
    , source : String
    , steps : Zipper (Element Msg)
    , errors : List DeadEnd
    , mode : Mode
    }


type Mode
    = Editing
    | Tutorial


init : () -> ( Model, Cmd Msg )
init () =
    ( { content =
            { questions = []
            , errors = []
            }
      , source = ""
      , steps =
            fromCons screenRecording
                []
      , errors = []
      , mode = Editing
      }
    , Cmd.none
    )


type Msg
    = SrcChanged String
    | Api (Result Http.Error String)
    | Download
    | Timestamp Time.Posix
    | Guide Step
    | Change Mode


type Step
    = Inc
    | Decr


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        { content, steps } =
            model
    in
    case msg of
        Change m ->
            ( { model | mode = m }
            , case m of
                Tutorial ->
                    Task.perform (\_ -> Download) Time.now

                Editing ->
                    Cmd.none
            )

        Guide step ->
            case step of
                Inc ->
                    ( { model | steps = tug next steps }, Cmd.none )

                Decr ->
                    ( { model | steps = tug previous steps }, Cmd.none )

        Download ->
            ( model
            , Task.perform Timestamp Time.now
            )

        Timestamp time ->
            let
                file : String
                file =
                    export content.questions
            in
            ( model
            , if file == "" then
                Cmd.none

              else
                let
                    filename : String
                    filename =
                        String.concat
                            [ "offlinequiz"
                            , String.fromInt <| Time.toYear Time.utc time
                            , String.fromInt <| toMonth <| Time.toMonth Time.utc time
                            , String.fromInt <| Time.toDay Time.utc time
                            , String.fromInt <| Time.toHour Time.utc time
                            , String.fromInt <| Time.toMinute Time.utc time
                            , ".gift"
                            ]
                in
                Download.string
                    filename
                    "text/plain"
                    file
            )

        SrcChanged src ->
            case
                Parser.run quiz src
            of
                Ok qz ->
                    ( { model | source = src, content = { content | questions = qz } }, Cmd.none )

                Err err ->
                    ( { model | errors = err }, Cmd.none )

        Api result ->
            case result of
                Ok src ->
                    ( { model | source = src }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )


view : Model -> Browser.Document Msg
view { steps, content, source, mode } =
    { title = "Moodle Question Editor"
    , body =
        [ layout [] <|
            case mode of
                Editing ->
                    column [ width fill, height fill ]
                        [ row [ height fill, width fill ]
                            [ editor
                                content.questions
                                source
                            , column
                                [ paddingEach { edges | left = 8, right = 8 }
                                , width fill
                                , height fill
                                ]
                                [ el
                                    [ paddingXY 0 12
                                    , bold
                                    ]
                                    (text "Preview: ")
                                , if length content.questions > 0 then
                                    viewSolutions content.questions

                                  else
                                    column
                                        [ Font.color <|
                                            rgb (136 / 255)
                                                (138 / 255)
                                                (133 / 255)
                                        , paddingXY 12 0
                                        ]
                                        [ text "use x's and o's to mark up the quiz."
                                        , text "x = true"
                                        , text "o = false"
                                        ]
                                , button [ alignBottom, centerX, padding 12 ]
                                    { label =
                                        el
                                            [ Border.width 2, alignBottom, padding 8 ]
                                            (text "Download and Continue")
                                    , onPress = Just (Change Tutorial)
                                    }
                                ]
                            ]
                        ]

                Tutorial ->
                    column [ centerX, centerY ]
                        [ current steps
                        , zipperNav [ padding 8, alignBottom, width fill ] steps
                        ]
        ]
    }


editor : List (List Bool) -> String -> Element Msg
editor xs s =
    let
        numcol : Element Msg
        numcol =
            column
                [ height fill
                , width <| px 36
                , paddingEach { edges | top = 20 }
                , spacing 10
                , Font.size 15
                , Font.family [ Font.monospace ]
                ]
                (List.map (el [ centerX ] << text << (\t -> t ++ ".") << String.fromInt << (\n -> 1 + n)) <|
                    range 0 (length xs)
                )
    in
    row [ height fill, width fill ]
        [ numcol
        , column [ height fill, width fill ]
            [ multiline [ height fill, width fill ]
                { onChange = SrcChanged
                , text = s
                , placeholder = Just (placeholder [] (text "xoxxo"))
                , label = labelHidden "Editor"
                , spellcheck = False
                }
            ]
        ]


viewSolutions : List (List Bool) -> Element msg
viewSolutions questions =
    let
        box : Bool -> Element msg
        box b =
            el [ padding 4 ] (defaultCheckbox b)
    in
    column [ width fill, height fill, clipX ]
        (List.indexedMap
            (\i qs ->
                row [ padding 2 ] <|
                    text (String.fromInt (i + 1) ++ ":")
                        :: List.map box
                            qs
            )
            questions
        )


screenRecording : Element msg
screenRecording =
    el [ centerX, centerY, width <| px 1000 ]
        (html <|
            Html.video
                [ controls True
                ]
                [ Html.source
                    [ src <| Vite.asset "/src/assets/import.mp4"
                    , type_ "video/mp4"
                    ]
                    []
                ]
        )


export : List (List Bool) -> String
export bs =
    let
        template : Int -> List Bool -> String
        template i ans =
            String.join "\n"
                [ "::" ++ "Title " ++ String.fromInt (i + 1)
                , "::" ++ "Question :" ++ "{"
                , ans |> answerKey
                , "}"
                ]
    in
    bs
        |> List.indexedMap (\i b -> template i b)
        |> String.join "\n\n"


answerKey : List Bool -> String
answerKey bools =
    let
        trues : List Bool -> Float
        trues bs =
            bs
                |> List.filter identity
                |> List.length
                |> toFloat

        percentage : List Bool -> Float
        percentage bs =
            100.0 / trues bs

        points : Bool -> Float -> Float
        points b =
            if b then
                identity

            else
                negate

        template : Int -> Bool -> String
        template i b =
            "~%"
                ++ (left 8 <| fromFloat <| points b (percentage bools))
                ++ "%Answer "
                ++ fromInt (i + 1)
    in
    bools
        |> List.indexedMap (\i b -> template i b)
        |> String.join "\n"


zipperNav : List (Attribute Msg) -> Zipper a -> Element Msg
zipperNav attrs z =
    let
        --navButton :
        navButton : (number -> Coloring -> Html.Html b) -> b -> Element b
        navButton icon msg =
            button [ width fill, alignLeft ]
                { label =
                    html <|
                        icon 48 (Color (Color.rgb255 0 0 0))
                , onPress = Just msg
                }

        back : Element Msg
        back =
            button
                [ centerX
                , width fill
                , Border.width 2
                ]
                { label =
                    el
                        [ centerX
                        , alignBottom
                        , padding 8
                        ]
                        (text "Back")
                , onPress = Just (Change Editing)
                }
    in
    row attrs
        [ when (not (isFirst z)) (navButton Filled.arrow_back (Guide Decr))
        , when (isFirst z) back
        , when (not (isLast z)) (navButton Filled.arrow_forward (Guide Inc))
        ]


when : Bool -> Element msg -> Element msg
when b c =
    if b then
        c

    else
        el [ width fill ] none


edges : { bottom : number, left : number, right : number, top : number }
edges =
    { bottom = 0, left = 0, right = 0, top = 0 }


toMonth : Month -> number
toMonth m =
    case m of
        Jan ->
            1

        Feb ->
            2

        Mar ->
            3

        Apr ->
            4

        May ->
            5

        Jun ->
            6

        Jul ->
            7

        Aug ->
            8

        Sep ->
            9

        Oct ->
            10

        Nov ->
            11

        Dec ->
            12


tug : (a -> Maybe a) -> a -> a
tug f a =
    Maybe.withDefault a (f a)
