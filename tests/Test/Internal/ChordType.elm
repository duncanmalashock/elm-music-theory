module Test.Internal.ChordType exposing (..)

import Expect
import Music.Internal.ChordType as ChordType
import Test exposing (Test, describe, test)


all : Test
all =
    describe "ChordType Tests"
        [ describe "isDominant"
            [ test "filtering ChordType.all for only dominant chords" <|
                \_ ->
                    let
                        expected =
                            [ ChordType.dominantEleventh
                            , ChordType.dominantNinth
                            , ChordType.dominantSeventh
                            , ChordType.augmentedDominantSeventh
                            , ChordType.dominantSeventhFlatFive
                            , ChordType.dominantSeventhFlatNine
                            , ChordType.dominantSeventhFlatNineFlatThirteen
                            , ChordType.dominantSeventhFlatNineSharpNine
                            , ChordType.dominantSeventhFlatNineSharpNineFlatThirteen
                            , ChordType.dominantSeventhSharpNine
                            , ChordType.dominantSeventhSharpNineFlatThirteen
                            , ChordType.dominantSeventhSus4
                            , ChordType.dominantThirteenth
                            , ChordType.dominantThirteenthFlatNine
                            , ChordType.dominantThirteenthSharpNine
                            , ChordType.dominantThirteenthSharpNineFlatNine
                            ]

                        result =
                            List.filter
                                ChordType.isDominant
                                ChordType.all
                    in
                    Expect.equal expected result
            ]
        , describe "isDiminished"
            [ test "filtering ChordType.all for only diminished chords" <|
                \_ ->
                    let
                        expected =
                            [ ChordType.diminished
                            , ChordType.diminishedSeventh
                            , ChordType.diminishedSeventhElevenFlatThirteen
                            , ChordType.halfDiminished
                            ]

                        result =
                            List.filter
                                ChordType.isDiminished
                                ChordType.all
                    in
                    Expect.equal expected result
            ]
        , describe "symbol"
            [ test "converts to symbols" <|
                \_ ->
                    let
                        expected =
                            [ ""
                            , "m"
                            , "aug"
                            , "dim"
                            , "sus2"
                            , "sus4"
                            , "6"
                            , "6/9"
                            , "m6"
                            , "m6/9"
                            , "(add9)"
                            , "m(add9)"

                            --
                            , "M7"
                            , "m7"
                            , "7"
                            , "o7"
                            , "ø7"
                            , "m/M7"

                            --
                            , "M9"
                            , "m9"
                            , "9"
                            , "m11"
                            , "11"
                            , "13"

                            --
                            , "7(♭9)"
                            , "7(♯9)"
                            , "7(♭9,♯9)"
                            , "7(♭9,♯11)"
                            , "7(♯9,♯11)"
                            , "7(♯11)"
                            , "7(♭9,♭13)"
                            , "7(♯9,♭13)"
                            , "7(♯11,♭13)"

                            --
                            , "M7(♯11)"
                            , "7(♯5)"
                            , "7sus4"
                            ]

                        result =
                            [ ChordType.major
                            , ChordType.minor
                            , ChordType.augmented
                            , ChordType.diminished
                            , ChordType.sus2
                            , ChordType.sus4
                            , ChordType.majorSix
                            , ChordType.majorSixNine
                            , ChordType.minorSix
                            , ChordType.minorSixNine
                            , ChordType.majorAddNine
                            , ChordType.minorAddNine

                            --
                            , ChordType.majorSeventh
                            , ChordType.minorSeventh
                            , ChordType.dominantSeventh
                            , ChordType.diminishedSeventh
                            , ChordType.halfDiminished
                            , ChordType.minorMajorSeventh

                            --
                            , ChordType.majorNinth
                            , ChordType.minorNinth
                            , ChordType.dominantNinth
                            , ChordType.minorEleventh
                            , ChordType.dominantEleventh
                            , ChordType.dominantThirteenth

                            --
                            , ChordType.dominantSeventhFlatNine
                            , ChordType.dominantSeventhSharpNine
                            , ChordType.dominantSeventhFlatNineSharpNine
                            , ChordType.dominantSeventhFlatNineSharpEleven
                            , ChordType.dominantSeventhSharpNineSharpEleven
                            , ChordType.dominantSeventhSharpEleven
                            , ChordType.dominantSeventhFlatNineFlatThirteen
                            , ChordType.dominantSeventhSharpNineFlatThirteen
                            , ChordType.dominantSeventhSharpElevenFlatThirteen

                            --
                            , ChordType.majorSeventhSharpEleven
                            , ChordType.augmentedDominantSeventh
                            , ChordType.dominantSeventhSus4
                            ]
                                |> List.map ChordType.toString
                    in
                    Expect.equal expected result
            ]
        ]
