port module Main exposing (main)

{-| A /very/ simple blog post with a custom inline element for some cool text formatting.

This is to get you started.

-}

import Browser
import Editor exposing (edit, editor)
import Element exposing (..)
import Element.Font as Font
import Html
import Html.Events exposing (on, onClick)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (requiredAt)
import Mark
import Mark.Error
import Parser exposing (Trailing(..))
import Question exposing (Question, exam, exportToGift)



-- | Copy to Clipboard


port copy : String -> Cmd msg


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
    , Cmd.none
    )


type Msg
    = SrcChanged String
    | Copy


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Copy ->
            ( model, model.questions |> exportToGift |> copy )

        SrcChanged src ->
            case Mark.compile exam src of
                Mark.Success qs ->
                    ( { model | questions = qs }
                    , Cmd.none
                    )

                Mark.Almost { result, errors } ->
                    -- This is the case where there has been an error,
                    -- but it has been caught by `Mark.onError` and is still rendereable.
                    ( { model | questions = result, errors = errors }
                    , Cmd.none
                    )

                Mark.Failure errors ->
                    ( { model | errors = errors }
                    , Cmd.none
                    )


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
                    ]
                , viewDocument model.questions
                , model.questions
                    |> exportToGift
                    |> viewGift
                , html <|
                    Html.button
                        [ onClick Copy ]
                        [ Html.text "copy" ]
                ]
        ]
    }


viewDocument : List Question -> Element Msg
viewDocument qs =
    Question.viewExam qs


srcDecoder : Decoder Msg
srcDecoder =
    Decode.succeed SrcChanged
        |> requiredAt [ "detail", "value" ] Decode.string
        |> Debug.log "hello"


viewErrors errors =
    List.map
        (Mark.Error.toHtml Mark.Error.Light >> html)
        errors
