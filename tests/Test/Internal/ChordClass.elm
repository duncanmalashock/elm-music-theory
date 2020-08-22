module Test.Internal.ChordClass exposing (..)

import Expect
import Internal.ChordClass as ChordClass
import Test exposing (Test, describe, test)


all : Test
all =
    describe "ChordClass Tests"
        [ describe "isDominant"
            [ test "filtering ChordClass.all for only dominant chords" <|
                \_ ->
                    let
                        expected =
                            [ ChordClass.dominantEleventh
                            , ChordClass.dominantNinth
                            , ChordClass.dominantSeventh
                            , ChordClass.dominantSeventhSharpFive
                            , ChordClass.dominantSeventhFlatFive
                            , ChordClass.dominantSeventhFlatNine
                            , ChordClass.dominantSeventhFlatNineFlatThirteen
                            , ChordClass.dominantSeventhFlatNineSharpNine
                            , ChordClass.dominantSeventhFlatNineSharpNineFlatThirteen
                            , ChordClass.dominantSeventhSharpNine
                            , ChordClass.dominantSeventhSharpNineFlatThirteen
                            , ChordClass.dominantSeventhSus4
                            , ChordClass.dominantThirteenth
                            , ChordClass.dominantThirteenthFlatNine
                            , ChordClass.dominantThirteenthSharpNine
                            , ChordClass.dominantThirteenthSharpNineFlatNine
                            ]

                        result =
                            List.filter
                                ChordClass.isDominant
                                ChordClass.all
                    in
                    Expect.equal expected result
            ]
        , describe "isDiminished"
            [ test "filtering ChordClass.all for only diminished chords" <|
                \_ ->
                    let
                        expected =
                            [ ChordClass.diminished
                            , ChordClass.diminishedSeventh
                            , ChordClass.diminishedSeventhElevenFlatThirteen
                            , ChordClass.halfDiminished
                            ]

                        result =
                            List.filter
                                ChordClass.isDiminished
                                ChordClass.all
                    in
                    Expect.equal expected result
            ]
        ]
