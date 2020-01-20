module MusicTheory.Letter exposing (Letter(..), index, indexToLetterAndSteps, letters, semitones, toString)

{-| A letter represents a note from the diatonic C major scale.
-}


type Letter
    = C
    | D
    | E
    | F
    | G
    | A
    | B


toString : Letter -> String
toString letter =
    case letter of
        C ->
            "C"

        D ->
            "D"

        E ->
            "E"

        F ->
            "F"

        G ->
            "G"

        A ->
            "A"

        B ->
            "B"


letters : List Letter
letters =
    [ C, D, E, F, G, A, B ]


semitones : Letter -> Int
semitones letter =
    case letter of
        C ->
            0

        D ->
            2

        E ->
            4

        F ->
            5

        G ->
            7

        A ->
            9

        B ->
            11


index : Letter -> Int
index letter =
    case letter of
        C ->
            0

        D ->
            1

        E ->
            2

        F ->
            3

        G ->
            4

        A ->
            5

        B ->
            6


indexToLetterAndSteps : Int -> ( Letter, Int )
indexToLetterAndSteps n =
    case n of
        0 ->
            ( C, 1 )

        1 ->
            ( D, 2 )

        2 ->
            ( E, 2 )

        3 ->
            ( F, 1 )

        4 ->
            ( G, 2 )

        5 ->
            ( A, 2 )

        6 ->
            ( B, 2 )

        other ->
            if other < 0 then
                indexToLetterAndSteps (other + 7)

            else
                indexToLetterAndSteps (other - 7)
