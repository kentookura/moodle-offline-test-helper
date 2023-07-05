port module Main exposing (main)

{-| A /very/ simple blog post with a custom inline element for some cool text formatting.

This is to get you started.

-}

import Browser
import Debug 
import Html exposing (div)
import Html.Attributes as Attr
import Html.Events exposing (onClick, on)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (requiredAt)
import Mark
import Mark.Error
import Parser exposing (Trailing(..))
import Question exposing (exam)
import Text exposing (text)

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
    }

init () =
    ( { source = "" }
    , Cmd.none
    )



type Msg
    = SrcChanged String
    | Copy


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Copy ->
            case Mark.compile exam model.source of
                Mark.Success qs ->
                    ( model
                    , qs
                        |> List.map Question.toGift
                        |> String.join "\n\n"
                        |> copy
                    )
                
                _ -> ( model, Cmd.none)

        SrcChanged src ->
            ( { model | source = src }
            , Cmd.none
            )





view : Model -> Browser.Document Msg
view model =
    { title = ""
    , body =
        [ case Mark.compile exam model.source of
            Mark.Success qs ->
                Html.div []
                    [ Html.div [] (List.map Question.viewQuestion qs)
                    ]

            Mark.Almost { result, errors } ->
                -- This is the case where there has been an error,
                -- but it has been caught by `Mark.onError` and is still rendereable.
                Html.div []
                    [ Html.div [] (viewErrors errors)
                     --, Html.div [] result.body
                    ]

            Mark.Failure errors ->
                Html.div [] []

        --(viewErrors errors)
        , div []
            [ Html.button [ onClick Copy ] [ Html.text "copy" ]
            ]
        , Html.node "wc-monaco-editor" 
            [ Attr.style "width" "500px"
            , Attr.style "height" "500px"
            , Attr.attribute "language" "javascript" 
            --, Attr.property "value" <| Encode.string model.source
            , on "contentChanged"
                <| Debug.log "idk"
                <| srcDecoder
            ] []
        ]
    }
      
srcDecoder : Decoder Msg
srcDecoder = 
    Decode.succeed SrcChanged 
        |> requiredAt ["detail", "value"] Decode.string

viewErrors errors =
    List.map
        (Mark.Error.toHtml Mark.Error.Light)
        errors