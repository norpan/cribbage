module Count exposing (..)

{-| This function counts the cribbage score of five cards.

The first card is the starter card, and the other four are the hand.

-}

import Card exposing (..)
import List


count : List Card -> Int
count cards =
    countFifteens cards
        + countRuns cards
        + countPairs cards
        + countFlush cards
        + countNobs cards


countFifteens : List Card -> Int
countFifteens cards =
    -- Check all combinations of cards for a sum of 15
    cards
        |> List.map value
        |> combinations
        |> List.filter (\x -> List.sum x == 15)
        |> List.length
        |> (*) 2


countRuns : List Card -> Int
countRuns cards =
    let
        -- Check all sorted combinations of cards for runs
        runs =
            cards
                |> List.map sequenceValue
                |> List.sort
                |> combinations
                |> List.filter (\list -> List.length list > 2 && isRun list)
                |> List.map List.length

        maxRun =
            runs |> List.maximum |> Maybe.withDefault 0

        -- Only count the runs of maximum length
        maxRuns =
            runs
                |> List.filter (\run -> run == maxRun)
    in
    List.sum maxRuns


countPairs : List Card -> Int
countPairs cards =
    let
        -- Check all combinations of cards for pairs
        pairs =
            cards
                |> List.map sequenceValue
                |> combinations
                |> List.filter isPair
                |> List.length
    in
    pairs * 2


countFlush : List Card -> Int
countFlush cards =
    case cards of
        [ Card starterSuit _, Card suit1 _, Card suit2 _, Card suit3 _, Card suit4 _ ] ->
            if starterSuit == suit1 && starterSuit == suit2 && starterSuit == suit3 && starterSuit == suit4 then
                5

            else if suit1 == suit2 && suit2 == suit3 && suit3 == suit4 then
                4

            else
                0

        _ ->
            0


countNobs : List Card -> Int
countNobs cards =
    case cards of
        (Card starterSuit _) :: rest ->
            let
                nobs =
                    List.filter (\(Card suit rank) -> rank == Jack && suit == starterSuit) rest
                        |> List.length
            in
            nobs

        _ ->
            0


isRun : List Int -> Bool
isRun cards =
    case cards of
        x :: y :: xs ->
            x + 1 == y && isRun (y :: xs)

        _ ->
            True


isPair : List Int -> Bool
isPair cards =
    case cards of
        [ x, y ] ->
            x == y

        _ ->
            False


value : Card -> Int
value (Card _ face) =
    case face of
        Ace ->
            1

        Two ->
            2

        Three ->
            3

        Four ->
            4

        Five ->
            5

        Six ->
            6

        Seven ->
            7

        Eight ->
            8

        Nine ->
            9

        Ten ->
            10

        Jack ->
            10

        Queen ->
            10

        King ->
            10


combinations : List a -> List (List a)
combinations list =
    case list of
        [] ->
            [ [] ]

        x :: xs ->
            let
                combs =
                    combinations xs
            in
            List.map ((::) x) combs ++ combs


sequenceValue : Card -> Int
sequenceValue (Card _ face) =
    case face of
        Ace ->
            1

        Two ->
            2

        Three ->
            3

        Four ->
            4

        Five ->
            5

        Six ->
            6

        Seven ->
            7

        Eight ->
            8

        Nine ->
            9

        Ten ->
            10

        Jack ->
            11

        Queen ->
            12

        King ->
            13
