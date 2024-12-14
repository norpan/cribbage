module Card exposing (..)

import Random
import Random.List


type Card
    = Card Suit Rank


cardToString : Card -> String
cardToString (Card suit rank) =
    rankToString rank ++ suitToString suit


type Suit
    = Clubs
    | Diamonds
    | Hearts
    | Spades


suitToString : Suit -> String
suitToString suit =
    case suit of
        Clubs ->
            "C"

        Diamonds ->
            "D"

        Hearts ->
            "H"

        Spades ->
            "S"


suitOrder : Suit -> Int
suitOrder suit =
    case suit of
        Clubs ->
            1

        Diamonds ->
            2

        Hearts ->
            3

        Spades ->
            4


type Rank
    = Ace
    | Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight
    | Nine
    | Ten
    | Jack
    | Queen
    | King


rankToString : Rank -> String
rankToString rank =
    case rank of
        Ace ->
            "A"

        Two ->
            "2"

        Three ->
            "3"

        Four ->
            "4"

        Five ->
            "5"

        Six ->
            "6"

        Seven ->
            "7"

        Eight ->
            "8"

        Nine ->
            "9"

        Ten ->
            "T"

        Jack ->
            "J"

        Queen ->
            "Q"

        King ->
            "K"


rankOrder : Rank -> Int
rankOrder rank =
    case rank of
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


type alias Deck =
    List Card


sortCards : List Card -> List Card
sortCards cards =
    List.sortBy (\(Card _ rank) -> rankOrder rank) cards


newDeck : Deck
newDeck =
    List.concatMap (\suit -> List.map (Card suit) allRanks) allSuits


randomHand : Random.Generator (List Card)
randomHand =
    Random.List.choices 5 newDeck
        |> Random.map Tuple.first


allSuits : List Suit
allSuits =
    [ Clubs, Diamonds, Hearts, Spades ]


allRanks : List Rank
allRanks =
    [ Ace, Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King ]
