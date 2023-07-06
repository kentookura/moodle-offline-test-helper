port module Main exposing (main)

{-| A /very/ simple blog post with a custom inline element for some cool text formatting.

This is to get you started.

-}

import Browser
import Editor exposing (editor)
import Element exposing (..)
import Element.Font as Font
import Html
import Html.Attributes as Attr
import Html.Events exposing (on, onClick)
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (requiredAt)
import Mark
import Mark.Error
import Parser exposing (Trailing(..))
import Question exposing (Question, exportToGift, quiz, viewQuiz)



-- | Copy to Clipboard


port copy : String -> Cmd msg


port notifyEditor : String -> Cmd msg


main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }


type alias Model =
    { source : String
    , questions : List Question
    , errors : List Mark.Error.Error
    }


init () =
    ( { source = "", questions = [], errors = [] }
    , Http.get
        { url = "/exams/example.emu"
        , expect = Http.expectString Api
        }
    )


type Msg
    = SrcChanged String
    | Api (Result Http.Error String)
    | Copy


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Copy ->
            ( model
            , model.questions 
                |> exportToGift 
                |> copy 
            )

        SrcChanged src ->
            ( compileAndSave quiz src model, Cmd.none )

        Api result ->
            case result of
                Ok src ->
                    --( compileAndSave quiz src model, Cmd.none)
                    ( compileAndSave quiz src model, notifyEditor src )

                Err err ->
                    let
                        _ =
                            Debug.log "err" err
                    in
                    ( model, Cmd.none )


compileAndSave : Mark.Document (List Question) -> String -> Model -> Model
compileAndSave quiz src model =
    case Mark.compile quiz src of
        Mark.Success qs ->
            { model | questions = qs, source = src }

        Mark.Almost { result, errors } ->
            -- This is the case where there has been an error,
            -- but it has been caught by `Mark.onError` and is still rendereable.
            { model | questions = result, errors = errors }

        Mark.Failure errors ->
            { model | errors = errors }


viewGift : String -> Element msg
viewGift src =
    el [ Font.family [ Font.monospace ] ] (text src)


view : Model -> Browser.Document Msg
view model =
    { title = ""
    , body =
        [ layout [ width fill, height fill ] <|
            row [ alignTop ]
                [ editor
                    [ on "contentChanged" <|
                        srcDecoder
                    , Attr.attribute "src" model.source
                    ]
                , viewQuiz model.questions
                , column []
                    [ html <|
                        Html.button
                            [ onClick Copy ]
                            [ Html.text "copy" ]
                    , model.questions
                        |> exportToGift
                        |> viewGift
                    ]
                ]
        ]
    }


srcDecoder : Decoder Msg
srcDecoder =
    Decode.succeed SrcChanged
        |> requiredAt [ "detail", "value" ] Decode.string
        |> Debug.log "Hello from elm"
