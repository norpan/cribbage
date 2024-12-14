module Main exposing (..)

import Browser
import Card exposing (..)
import Count
import Html exposing (..)
import Html.Attributes exposing (..)
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
        [ div [] [ text (String.fromInt (Count.count model.hand).pairs) ]
        , div [] [ text (String.fromInt (Count.count model.hand).total) ]
        , div [] [ text (String.fromInt model.submittedScore) ]
        , viewCards model.hand
        , viewScores model
        ]


viewCards : List Card -> Html Msg
viewCards cards =
    case cards of
        [] ->
            text ""

        card :: rest ->
            div []
                [ div
                    [ style "width" "100%"
                    , style "display" "flex"
                    ]
                    [ viewCard card ]
                , div
                    [ style "width" "100%"
                    , style "display" "flex"
                    ]
                    (List.map viewCard (sortCards rest))
                ]


viewCard : Card -> Html msg
viewCard card =
    img [ style "width" "25%", class "card", src ("cards/" ++ cardToString card ++ ".svg") ] []


viewScores : Model -> Html Msg
viewScores model =
    table [ style "width" "100%" ]
        [ viewScoreRow [ 0, 1, 2, 3, 4, 5 ]
        , viewScoreRow [ 6, 7, 8, 9, 10, 11 ]
        , viewScoreRow [ 12, 13, 14, 15, 16, 17 ]
        , viewScoreRow [ 18, -1, 20, 21, 22, 23 ]
        , viewScoreRow [ 24, -1, -1, -1, 28, 29 ]
        ]


viewScoreRow : List Int -> Html Msg
viewScoreRow scores =
    tr [] (List.map viewScore scores)


viewScore : Int -> Html Msg
viewScore score =
    if score == -1 then
        td [] []

    else
        td [ onClick (SubmitScore score) ] [ button [ style "width" "100%", style "height" "40px", style "font-size" "24px" ] [ text (String.fromInt score) ] ]
