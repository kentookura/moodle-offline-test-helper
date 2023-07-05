port module Main exposing (main)

{-| A /very/ simple blog post with a custom inline element for some cool text formatting.

This is to get you started.

-}

import Browser
import Debug exposing (log)
import Html exposing (Html, Attribute, button, div)
import Html.Attributes as Attr
import Html.Events exposing (onClick, on)
import Json.Decode as Decode exposing (int, Decoder)
import Json.Decode.Pipeline exposing (requiredAt)
import Json.Encode as Encode
import Http
import Mark
import Mark.Error
import Parser exposing (DeadEnd, Parser, Trailing(..), keyword, oneOf, sequence, spaces)
import Platform
import Question exposing (exam, question)
import Text exposing (text)


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

                Mark.Almost { result, errors } ->
                    ( model, copy "almost" )

                -- This is the case where there has been an error,
                -- but it has been caught by `Mark.onError` and is still rendereable.
                Mark.Failure errors ->
                    ( model, errors |> List.map Mark.Error.toString |> String.join "\n" |> copy )

        --(model, copy ())
        SrcChanged src ->
            ( { model | source = src }
            , Cmd.none
            )


port copy : String -> Cmd msg



-- VIEW


main =
    --Platform.worker
    --  { init = init
    --  , update = update
    --  , subscriptions = always Sub.none}
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
                    [--  Html.div [] (viewErrors errors)
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



--document =
--    Mark.documentWith
--        (\meta body ->
--            { metadata = meta
--            , body =
--                Html.node "style" [] [ Html.text stylesheet ]
--                    :: Html.h1 [] meta.title
--                    :: body
--            }
--        )
--        -- We have some required metadata that starts our document.
--        { metadata = metadata
--        , body =
--            Mark.manyOf
--                [ header
--                , image
--                , list
--                , code
--                , Mark.map Question.viewQuestion question
--                , Mark.map (Html.p []) text
--                ]
--        }
{- Handle Metadata -}


metadata =
    Mark.record "Exam"
        (\author description title ->
            { author = author
            , description = description
            , title = title
            }
        )
        |> Mark.field "author" Mark.string
        |> Mark.field "description" text
        |> Mark.field "title" text
        |> Mark.toBlock



{- Handle Blocks -}


header =
    Mark.block "H1"
        (\children ->
            Html.h1 []
                children
        )
        text



--question : Mark.Block (Html msg)


image =
    Mark.record "Image"
        (\src description ->
            Html.img
                [ Attr.src src
                , Attr.alt description
                , Attr.style "float" "left"
                , Attr.style "margin-right" "48px"
                ]
                []
        )
        |> Mark.field "src" Mark.string
        |> Mark.field "description" Mark.string
        |> Mark.toBlock


code =
    Mark.block "Code"
        (\str ->
            Html.pre
                [ Attr.style "padding" "12px"
                , Attr.style "background-color" "#eee"
                ]
                [ Html.text str ]
        )
        Mark.string



{- Handling bulleted and numbered lists -}


list : Mark.Block (Html msg)
list =
    Mark.tree "List" renderList (Mark.map (Html.div []) text)



-- Note: we have to define this as a separate function because
-- `Items` and `Node` are a pair of mutually recursive data structures.
-- It's easiest to render them using two separate functions:
-- renderList and renderItem


renderList : Mark.Enumerated (Html msg) -> Html msg
renderList (Mark.Enumerated enum) =
    let
        group =
            case enum.icon of
                Mark.Bullet ->
                    Html.ul

                Mark.Number ->
                    Html.ol
    in
    group []
        (List.map renderItem enum.items)


renderItem : Mark.Item (Html msg) -> Html msg
renderItem (Mark.Item item) =
    Html.li []
        [ Html.div [] item.content
        , renderList item.children
        ]



{- Article Source

   Note: Here we're defining our source inline, but checkout the External Files example.

   External files have a syntax highlighter in VS Code, and a CLI utility to check for errors.  It's a way better experience!

-}
-- [Lorem Ipsum is simply---]{drop}}dummy text of the printing and [typesetting industry]{link| url = http://mechanical-elephant.com}. Lorem Ipsum has been the industry's /standard/ dummy text ever since the 1500's, when an "unknown printer" took a galley of type and scrambled it to<>make a type specimen book. It has survived not only five centuries, but also the leap into electronic typesetting, remaining essentially unchanged. It was *popularised* in the 1960's with the release of Letraset sheets containing Lorem Ipsum passages, and more recently with desktop publishing software like Aldus PageMaker including versions of Lorem Ipsum.
