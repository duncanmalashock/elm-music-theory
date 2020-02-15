module Generate.Voicing exposing (all)

import Expect
import MusicTheory.Chord as Chord
import MusicTheory.ChordClass as ChordClass
import MusicTheory.Generate.Voicing as GenerateVoicing exposing (VoicingError(..))
import MusicTheory.Interval as Interval
import MusicTheory.Letter exposing (Letter(..))
import MusicTheory.PitchClass as PitchClass exposing (natural)
import Test exposing (..)


all : Test
all =
    describe "all"
        [ describe "fourWayClose"
            [ test "All valid voicings for C major 6/9 chord" <|
                \_ ->
                    let
                        cMajorSixNineChord =
                            Chord.chord (PitchClass.pitchClass C natural) ChordClass.majorSixNine

                        expected =
                            Ok 66

                        result =
                            GenerateVoicing.fourWayClose cMajorSixNineChord
                                |> Result.map List.length
                    in
                    Expect.equal expected result
            , test "Should not be able to create voicing plans for chord without all four voice categories" <|
                \_ ->
                    let
                        cMajorChord =
                            Chord.chord (PitchClass.pitchClass C natural) ChordClass.major

                        expected =
                            Err MissingVoiceCategory

                        result =
                            GenerateVoicing.fourWayClose cMajorChord
                    in
                    Expect.equal expected result
            , test "Should not be able to create voicings for non-tertian chords" <|
                \_ ->
                    let
                        nonTertianChordClass =
                            ChordClass.nonTertian [ Interval.perfectUnison, Interval.perfectFifth ]

                        cMajorChord =
                            Chord.chord (PitchClass.pitchClass C natural) nonTertianChordClass

                        expected =
                            Err <| CantVoiceNonTertianChord <| ChordClass.ChordClassIsNonTertian nonTertianChordClass

                        result =
                            GenerateVoicing.fourWayClose cMajorChord
                    in
                    Expect.equal expected result
            ]
        ]
