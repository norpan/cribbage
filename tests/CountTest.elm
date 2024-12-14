module CountTest exposing (..)

import Card exposing (..)
import Count exposing (count)
import Expect exposing (..)
import Random
import Random.List
import Test exposing (..)


countTests : Test
countTests =
    describe "countTests"
        [ test "simple fifteen" <|
            \() ->
                let
                    cards =
                        [ Card Spades Ace
                        , Card Hearts Four
                        , Card Clubs Ten
                        , Card Clubs Nine
                        , Card Clubs Seven
                        ]
                in
                Expect.equal 2 (count cards)
        , test "no fifteens" <|
            \() ->
                let
                    cards =
                        [ Card Spades Ace
                        , Card Hearts Three
                        , Card Clubs King
                        , Card Clubs Nine
                        , Card Clubs Seven
                        ]
                in
                Expect.equal 0 (count cards)
        , test "sequence of three and a pair" <|
            \() ->
                let
                    cards =
                        [ Card Spades Ace
                        , Card Hearts Ace
                        , Card Clubs Two
                        , Card Clubs Three
                        , Card Clubs Five
                        ]
                in
                Expect.equal 8 (count cards)
        , test "sequence of three and and two pairs" <|
            \() ->
                let
                    cards =
                        [ Card Spades Ace
                        , Card Hearts Ace
                        , Card Clubs Two
                        , Card Clubs Three
                        , Card Hearts Three
                        ]
                in
                Expect.equal 16 (count cards)
        , test "four flush" <|
            \() ->
                let
                    cards =
                        [ Card Hearts Two
                        , Card Spades Four
                        , Card Spades Six
                        , Card Spades Eight
                        , Card Spades Ten
                        ]
                in
                Expect.equal 4 (count cards)
        , test "five flush" <|
            \() ->
                let
                    cards =
                        [ Card Spades Two
                        , Card Spades Four
                        , Card Spades Six
                        , Card Spades Eight
                        , Card Spades Ten
                        ]
                in
                Expect.equal 5 (count cards)
        , test "28" <|
            \() ->
                let
                    cards =
                        [ Card Spades Five
                        , Card Hearts Five
                        , Card Clubs Five
                        , Card Diamonds Five
                        , Card Diamonds Jack
                        ]
                in
                Expect.equal 28 (count cards)
        , test "maximum 29" <|
            \() ->
                let
                    cards =
                        [ Card Spades Five
                        , Card Hearts Five
                        , Card Clubs Five
                        , Card Diamonds Five
                        , Card Spades Jack
                        ]
                in
                Expect.equal 29 (count cards)
        , test "Random hand" <|
            \() ->
                let
                    seed =
                        Random.initialSeed 1234

                    handGenerator =
                        Random.List.choices 5 Card.newDeck

                    ( ( cards, _ ), _ ) =
                        Random.step handGenerator seed
                in
                Expect.equal 10 (count cards)
        ]
