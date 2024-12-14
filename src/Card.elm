module Card exposing (..)

import Random
import Random.List


type Card
    = Card Suit Rank


type Suit
    = Clubs
    | Diamonds
    | Hearts
    | Spades


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


type alias Deck =
    List Card


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
