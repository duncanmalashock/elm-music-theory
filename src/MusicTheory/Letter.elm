module MusicTheory.Letter exposing
    ( Letter(..)
    , index
    , nextWithSemitoneCount
    , prevWithSemitoneCount
    , semitones
    , toString
    )

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


nextWithSemitoneCount : ( Letter, Int ) -> ( Letter, Int )
nextWithSemitoneCount ( letter, semis ) =
    case letter of
        C ->
            ( D, semis + 2 )

        D ->
            ( E, semis + 2 )

        E ->
            ( F, semis + 1 )

        F ->
            ( G, semis + 2 )

        G ->
            ( A, semis + 2 )

        A ->
            ( B, semis + 2 )

        B ->
            ( C, semis + 1 )


prevWithSemitoneCount : ( Letter, Int ) -> ( Letter, Int )
prevWithSemitoneCount ( letter, semis ) =
    case letter of
        C ->
            ( B, semis - 1 )

        B ->
            ( A, semis - 2 )

        A ->
            ( G, semis - 2 )

        G ->
            ( F, semis - 2 )

        F ->
            ( E, semis - 1 )

        E ->
            ( D, semis - 2 )

        D ->
            ( C, semis - 2 )
