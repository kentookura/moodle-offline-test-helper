port module Main exposing (main)

{-| A /very/ simple blog post with a custom inline element for some cool text formatting.

This is to get you started.

-}

import Browser
import Color
import Editor exposing (editor)
import Element exposing (..)
import Element.Border as Border
import Element.Font as Font exposing (bold)
import Element.Input exposing (button, defaultCheckbox)
import File.Download as Download
import Html.Attributes as Attr
import Html.Events exposing (on)
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (requiredAt)
import List.Zipper exposing (Zipper, current, fromCons, isFirst, isLast, next, previous)
import Mark.Edit exposing (Edit)
import Mark.Error
import Material.Icons as Filled
import Material.Icons.Types exposing (Coloring(..))
import Parser exposing (DeadEnd, Trailing(..))
import Parsing exposing (quiz)
import String exposing (fromFloat, fromInt, left)
import Task
import Time exposing (Month(..))


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
    , helpVisible : Bool
    , steps : Zipper (Element Msg)
    , errors : List DeadEnd
    , mode : Mode
    }


type Mode
    = Editing
    | Tutorial


port copy : String -> Cmd msg


init : () -> ( Model, Cmd Msg )
init () =
    let
        initialContent =
            { questions = []
            , errors = []
            }
    in
    ( { content = initialContent
      , source = ""
      , helpVisible = False
      , steps =
            fromCons step1 [ step2 ]
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
    | ToggleHelp
    | Guide Step
    | Change Mode


type Step
    = Inc
    | Decr


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        { helpVisible, content, steps, mode } =
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
            , Cmd.batch [ Task.perform Timestamp Time.now ]
              --content.questions
              --    |> toMoodleDefault
              --    |> save timestamp
            )

        Timestamp time ->
            let
                filename =
                    String.join ""
                        [ "offlinequiz"
                        , String.fromInt <| Time.toYear Time.utc time
                        , String.fromInt <| toMonth <| Time.toMonth Time.utc time
                        , String.fromInt <| Time.toDay Time.utc time
                        , String.fromInt <| Time.toHour Time.utc time
                        , String.fromInt <| Time.toMinute Time.utc time
                        , ".gift"
                        ]
            in
            ( model
            , Download.string
                filename
                "text/plain"
                (content.questions |> toGift)
            )

        -- save : Time.Posix -> String -> Cmd msg
        -- save time gift =
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

        ToggleHelp ->
            ( { model | helpVisible = not helpVisible }, Cmd.none )


view : Model -> Browser.Document Msg
view { steps, content, source, mode } =
    { title = "Edit Questions"
    , body =
        [ layout [] <|
            case mode of
                Editing ->
                    column [ width fill, height fill ]
                        [ row [ height fill, width fill ]
                            [ el [ height fill, width fill ]
                                (editor
                                    [ on "contentChanged" <|
                                        srcDecoder
                                    , Attr.attribute "src" source
                                    ]
                                )
                            , column [ paddingEach { edges | left = 8, right = 8 }, width fill, height fill ]
                                [ el [ bold ] (text "Preview: ")
                                , content.questions
                                    |> viewAnswerkey
                                , button [] { label = el [ alignBottom, padding 8 ] (text "Download and Continue"), onPress = Just (Change Tutorial) }
                                ]
                            ]
                        ]

                Tutorial ->
                    column []
                        [ current steps
                        , zipperNav [ alignBottom, width fill ] steps
                        ]
        ]
    }


zipperNav : List (Attribute Msg) -> Zipper a -> Element Msg
zipperNav attrs z =
    let
        navButton icon msg =
            button [ width fill, alignLeft ]
                { label =
                    html <|
                        icon 48 (Color (Color.rgb255 0 0 0))
                , onPress = Just msg
                }

        back =
            button [ width fill ] { label = text "Edit", onPress = Just (Change Editing) }
    in
    row attrs
        [ when (not (isFirst z)) (navButton Filled.arrow_back (Guide Decr))
        , when (isFirst z) back
        , when (not (isLast z)) (navButton Filled.arrow_forward (Guide Inc))
        ]


toGift : List (List Bool) -> String
toGift bs =
    let
        template i ans =
            String.join "\n"
                [ "::" ++ "Title " ++ String.fromInt i
                , "::" ++ "Question :" ++ "{"
                , ans |> answerKey
                , "}"
                ]
    in
    bs
        |> List.indexedMap (\i b -> template i b)
        --|> List.map toMoodle
        |> String.join "\n\n"


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


when : Bool -> Element msg -> Element msg
when b c =
    if b then
        c

    else
        el [ width fill ] none


viewAnswerkey : List (List Bool) -> Element msg
viewAnswerkey questions =
    let
        box b =
            el [ padding 4 ] (defaultCheckbox b)
    in
    column []
        (List.indexedMap
            (\i qs ->
                column []
                    [ text ("Question " ++ String.fromInt i ++ ":")
                    , row [ padding 8 ]
                        (List.map
                            box
                            qs
                        )
                    ]
            )
            questions
        )


step1 : Element msg
step1 =
    paragraph
        []
        [ text "Use the editor to mark up your answer key. For example "
        , el [ Font.family [ Font.monospace ] ]
            (text "tftt")
        , text " corresponds to a question with 4 possible answers, with option 1, 3 and 4 are correct. Add more lines for more questions. After downloading, go to your offline test in Moodle:"
        , image [ width <| px 400 ]
            { src = "step0.png"
            , description = "Screenshot of Moodle."
            }
        ]


step2 : Element msg
step2 =
    paragraph [ padding 8 ]
        [ text "Under \"More\", navigate to the question bank. Then click on \"Import\""
        , el [ Border.color (rgb 0 0.7 0), height shrink ]
            (image []
                { src = "select_import.png"
                , description = "Screenshot of Moodle."
                }
            )
        ]


srcDecoder : Decoder Msg
srcDecoder =
    Decode.succeed SrcChanged
        |> requiredAt [ "detail", "value" ] Decode.string


edges : { top : number, left : number, right : number, bottom : number }
edges =
    { top = 0, left = 0, right = 0, bottom = 0 }


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
