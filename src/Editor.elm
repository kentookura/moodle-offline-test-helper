module Editor exposing (..)

import Element exposing (Element, html)
import Html exposing (Attribute, Html)
import Html.Attributes as Attr


editor : List (Attribute msg) -> Element msg



--()


editor attrs =
    html <|
        Html.node "wc-monaco-editor"
            ([ Attr.style "width" "500px"
             , Attr.style "height" "500px"
             , Attr.attribute "language" "javascript"

             --, Attr.property "value" <| Encode.string model.source
             ]
                ++ attrs
            )
            []