module Text exposing (..)

import Browser
import Element as UI exposing (Element)
import Html.Attributes as Attr
import Html.Events exposing (onClick)
import Http
import Mark exposing (..)
import Mark.Error
import Parser exposing (..)


type Math
    = Math


parseMath =
    getChompedString <|
        succeed Math
            |. symbol "$"
            |. chompUntil "$"


text : Mark.Block (List (Element msg))
text =
    Mark.textWith
        { view =
            \styles string ->
                viewText styles string
        , replacements = Mark.commonReplacements
        , inlines =
            [ Mark.annotation "link"
                (\styles url -> UI.link [] { url = url, label = UI.text "asfd" }
                 --UI.link [] {url = url, label = styles }--(List.map (applyTuple viewText) texts)}
                 --Html.a [ Attr.href url ] (List.map (applyTuple viewText) texts)
                )
                |> Mark.field "url" Mark.string
            , Mark.verbatim "drop"
                (\str ->
                    let
                        drop =
                            String.left 1 str

                        lede =
                            String.dropLeft 1 str
                    in
                    UI.el [] (UI.text str)
                 --[ Html.span [ Attr.class "drop-capital" ]
                 --    [ Html.text drop ]
                 --, Html.span [ Attr.class "lede" ]
                 --    [ Html.text lede ]
                 --]
                )
            , Mark.verbatim "math"
                (\str ->
                    str
                        |> run parseMath
                        |> Result.mapError
                            (\_ -> mathError)
                        |> (\x ->
                                case x of
                                    Ok s ->
                                        UI.text s

                                    Err _ ->
                                        UI.none
                           )
                )
            ]
        }


mathError =
    Mark.Error.Custom "" [ "" ]


applyTuple fn ( one, two ) =
    fn one two


viewText styles string =
    if styles.bold || styles.italic || styles.strike then
        UI.el
            []
            --[ Attr.classList
            --    [ ( "bold", styles.bold )
            --    , ( "italic", styles.italic )
            --    , ( "strike", styles.strike )
            --    ]
            --]
            (UI.text string)

    else
        UI.text string
