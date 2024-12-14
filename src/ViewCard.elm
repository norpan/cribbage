module ViewCard exposing (..)

import Card exposing (Card)
import Html exposing (..)


viewCard : Card -> Html msg
viewCard card =
    div []
        [ text (Debug.toString card)
        ]
