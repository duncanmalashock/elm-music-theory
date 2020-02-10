module Analyze.ChordClass exposing (all)

import Expect
import MusicTheory.Analyze.ChordClass as AnalyzeChordClass
import MusicTheory.ChordClass as ChordClass
import MusicTheory.ScaleClass as ScaleClass
import Test exposing (..)


all : Test
all =
    describe "all"
        [ describe "isInScaleClass"
            [ test "Major seventh chord should be in major scale" <|
                \_ ->
                    let
                        majorScaleClass =
                            ScaleClass.major

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
                            , ScaleClass.Dorian
                            , ScaleClass.Phrygian
                            , ScaleClass.Minor
                            ]

                        minorSeventhChordClass =
                            ChordClass.minorSeventh

                        chordClassesForScaleClass =
                            AnalyzeChordClass.scaleClassesFor minorSeventhChordClass
                    in
                    Expect.equal chordClassesForScaleClass expectedScaleClasses
            ]
        ]
