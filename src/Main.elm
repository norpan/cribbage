module Main exposing (..)

import Browser
import Card exposing (..)
import Count
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Random exposing (generate)


type alias Model =
    { hand : List Card, scores : Maybe ( Int, Count.CountResult ) }


type Msg
    = GenerateHand
    | NewHand (List Card)
    | SubmitScore Int


main : Program () Model Msg
main =
    Browser.element { init = init, update = update, subscriptions = subscriptions, view = view }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { hand = [], scores = Nothing }, generateHand )


generateHand : Cmd Msg
generateHand =
    generate NewHand randomHand


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GenerateHand ->
            ( { model | scores = Nothing }, generateHand )

        NewHand hand ->
            ( { model | hand = hand }, Cmd.none )

        SubmitScore score ->
            ( { model | scores = Just ( score, Count.count model.hand ) }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html Msg
view model =
    div [ style "font-size" "32px", style "font-family" "sans-serif" ]
        [ viewCards model
        , viewScoreButtons model
        ]


viewCards : Model -> Html Msg
viewCards model =
    case model.hand of
        [] ->
            text ""

        card :: rest ->
            div []
                [ div
                    [ style "width" "100%"
                    , style "display" "flex"
                    , style "gap" "10px"
                    ]
                    [ viewCard card, viewResult model ]
                , div
                    [ style "width" "100%"
                    , style "display" "flex"
                    ]
                    (List.map viewCard (sortCards rest))
                ]


viewResult : Model -> Html Msg
viewResult model =
    case model.scores of
        Just ( submittedScore, handScore ) ->
            div
                [ style "color"
                    (if submittedScore == handScore.total then
                        "green"

                     else
                        "red"
                    )
                ]
                [ div [] [ text ("Your calculation: " ++ String.fromInt submittedScore) ]
                , div [] [ text ("Actual score: " ++ String.fromInt handScore.total) ]
                , div [] [ text ("Fifteens: " ++ String.fromInt handScore.fifteens) ]
                , div [] [ text ("Runs: " ++ String.fromInt handScore.runs) ]
                , div [] [ text ("Pairs: " ++ String.fromInt handScore.pairs) ]
                , div [] [ text ("Flush: " ++ String.fromInt handScore.flush) ]
                , div [] [ text ("Nobs: " ++ String.fromInt handScore.nobs) ]
                ]

        _ ->
            text "Calculate score!"


viewCard : Card -> Html msg
viewCard card =
    img [ style "width" "25%", class "card", src ("cards/" ++ cardToString card ++ ".svg") ] []


viewScoreButtons : Model -> Html Msg
viewScoreButtons model =
    table [ style "table-layout" "fixed", style "width" "100%" ]
        [ viewScoreButtonRow model [ 0, 1, 2, 3, 4, 5 ]
        , viewScoreButtonRow model [ 6, 7, 8, 9, 10, 11 ]
        , viewScoreButtonRow model [ 12, 13, 14, 15, 16, 17 ]
        , viewScoreButtonRow model [ 18, -1, 20, 21, 22, 23 ]
        , viewScoreButtonRow model [ 24, -1, -2, -1, 28, 29 ]
        ]


viewScoreButtonRow : Model -> List Int -> Html Msg
viewScoreButtonRow model scores =
    tr [] (List.map (viewButtonScore model) scores)


viewButtonScore : Model -> Int -> Html Msg
viewButtonScore model score =
    let
        showButtons =
            model.scores == Nothing
    in
    if score < -1 then
        td [ onClick GenerateHand ]
            [ button
                [ disabled showButtons
                , style "width" "100%"
                , style "height" "80px"
                , style "font-size" "40"
                ]
                [ text "New" ]
            ]

    else if score == -1 then
        td [] []

    else
        td [ onClick (SubmitScore score) ]
            [ button
                [ disabled (not showButtons)
                , style "width" "100%"
                , style "height" "80px"
                , style "font-size" "40"
                ]
                [ text (String.fromInt score) ]
            ]
