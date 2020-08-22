module Test.Internal.ChordType exposing (..)

import Expect
import MusicTheory.Internal.ChordType as ChordType
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
                            , ChordType.dominantSeventhSharpFive
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
        ]
