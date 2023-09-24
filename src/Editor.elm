module Editor exposing (..)

import Element exposing (..)
import Html exposing (Attribute)
import Html.Attributes as Attr


editor : List (Attribute msg) -> Element msg
editor attrs =
    el [ width fill, height fill ]
        (html <|
            Html.node "wc-monaco-editor"
                ([ Attr.attribute "language" "javascript"

                 --, Attr.property "value" <| Encode.string model.source
                 ]
                    ++ attrs
                )
                []
        )
