module Main exposing (..)

import Browser
import Card exposing (..)
import Count
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Random exposing (generate)
import ViewCard exposing (..)


type alias Model =
    { hand : List Card, submittedScore : Int }


type Msg
    = NoOp
    | NewHand (List Card)
    | SubmitScore Int


main : Program () Model Msg
main =
    Browser.element { init = init, update = update, subscriptions = subscriptions, view = view }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { hand = [], submittedScore = 0 }, generateHand )


generateHand : Cmd Msg
generateHand =
    generate NewHand randomHand


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        NewHand hand ->
            ( { model | hand = hand }, Cmd.none )

        SubmitScore score ->
            ( { model | submittedScore = score }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html Msg
view model =
    div []
        [ div [] [ text (String.fromInt (Count.count model.hand)) ]
        , div [] [ text (String.fromInt model.submittedScore) ]
        , div [] (List.map viewCard model.hand)
        , viewScores model
        ]


viewScores : Model -> Html Msg
viewScores model =
    table []
        [ tr [] (List.map viewScore [ 1, 2, 3, 4, 5, 6 ])
        , tr [] (List.map viewScore [ 7, 8, 9, 10, 11, 12 ])
        , tr [] (List.map viewScore [ 13, 14, 15, 16, 17, 18 ])
        , tr [] (List.map viewScore [ 0, 20, 21, 22, 23, 24 ])
        , tr [] (List.map viewScore [ 0, 0, 0, 0, 29, 0 ])
        ]


viewScore : Int -> Html Msg
viewScore score =
    if score == 0 then
        td [] []

    else
        td [ onClick (SubmitScore score) ] [ button [ style "width" "100%" ] [ text (String.fromInt score) ] ]
