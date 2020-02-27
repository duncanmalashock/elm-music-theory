module Test.Analyze.ChordClass exposing (all)

import Expect
import MusicTheory.Analyze.ChordClass as AnalyzeChordClass
import MusicTheory.ChordClass as ChordClass
import MusicTheory.ScaleClass as ScaleClass
import MusicTheory.TertianFactors exposing (TertianFactor(..))
import Test exposing (..)


all : Test
all =
    describe "all"
        [ describe "isInScaleClass"
            [ test "Major seventh chord should be in major scale" <|
                \_ ->
                    let
                        majorScaleClass =
                            ScaleClass.ionian

                        majorSeventhChordClass =
                            ChordClass.majorSeventh

                        chordClassIsInScaleClass =
                            AnalyzeChordClass.isInScaleClass majorScaleClass majorSeventhChordClass
                    in
                    Expect.equal chordClassIsInScaleClass True
            , test "Minor seventh chord should be in dorian scale" <|
                \_ ->
                    let
                        dorianScaleClass =
                            ScaleClass.dorian

                        minorSeventhChordClass =
                            ChordClass.minorSeventh

                        chordClassIsInScaleClass =
                            AnalyzeChordClass.isInScaleClass dorianScaleClass minorSeventhChordClass
                    in
                    Expect.equal chordClassIsInScaleClass True
            , test "Minor seventh chord should be in aeolian scale" <|
                \_ ->
                    let
                        aeolianScaleClass =
                            ScaleClass.aeolian

                        minorSeventhChordClass =
                            ChordClass.minorSeventh

                        chordClassIsInScaleClass =
                            AnalyzeChordClass.isInScaleClass aeolianScaleClass minorSeventhChordClass
                    in
                    Expect.equal chordClassIsInScaleClass True
            , test "Half diminished chord should be in locrian scale" <|
                \_ ->
                    let
                        locrianScaleClass =
                            ScaleClass.locrian

                        halfDiminishedChordClass =
                            ChordClass.halfDiminished

                        chordClassIsInScaleClass =
                            AnalyzeChordClass.isInScaleClass locrianScaleClass halfDiminishedChordClass
                    in
                    Expect.equal chordClassIsInScaleClass True
            ]
        , describe "scaleClassesFor"
            [ test "Minor seventh chord should be in the correct scales" <|
                \_ ->
                    let
                        expectedScaleClasses =
                            [ ScaleClass.Aeolian
                            , ScaleClass.DiminishedHalfToneWholeTone
                            , ScaleClass.Dorian
                            , ScaleClass.LydianDiminished
                            , ScaleClass.MinorPentatonic
                            , ScaleClass.Phrygian
                            ]

                        minorSeventhChordClass =
                            ChordClass.minorSeventh

                        chordClassesForScaleClass =
                            AnalyzeChordClass.scaleClassesFor minorSeventhChordClass
                    in
                    Expect.equal chordClassesForScaleClass expectedScaleClasses
            ]
        , describe "taggedIntervalsInRootCategoryFor"
            [ test "Correct tagged intervals for each category for a dominant 7 b9 b13 chord" <|
                \_ ->
                    let
                        expected =
                            Ok
                                { root =
                                    [ Root
                                    , FlatNinth
                                    ]
                                , third =
                                    [ MajorThird
                                    ]
                                , fifth =
                                    [ FlatThirteenth
                                    ]
                                , seventh =
                                    [ MinorSeventh
                                    ]
                                }

                        dominantSeventhFlatNineFlatThirteenChordClass =
                            ChordClass.dominantSeventhFlatNineFlatThirteen

                        result =
                            AnalyzeChordClass.tertianFactorsByCategory dominantSeventhFlatNineFlatThirteenChordClass
                    in
                    Expect.equal expected result
            ]
        , describe "inScaleClass"
            [ test "Chords contained in lydian scale (from root)" <|
                \_ ->
                    let
                        expected =
                            [ ChordClass.major
                            , ChordClass.majorAddNine
                            , ChordClass.majorSeventh
                            , ChordClass.majorSix
                            , ChordClass.majorSixNine
                            , ChordClass.sus2
                            , ChordClass.majorSeventhSharpEleven
                            ]

                        result =
                            AnalyzeChordClass.inScaleClass ScaleClass.lydian
                    in
                    Expect.equal expected result
            ]
        ]