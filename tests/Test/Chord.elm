module Test.Chord exposing (all)

import Expect
import MusicTheory.Chord as Chord
import MusicTheory.ChordClass as ChordClass
import MusicTheory.Inversion as Inversion
import MusicTheory.Octave as Octave
import MusicTheory.Pitch as Pitch
import MusicTheory.PitchClass as PitchClass
import Test exposing (Test, describe, test)


all : Test
all =
    describe "all"
        [ describe "toPitchClasses"
            [ test "Should return C E G from C major triad" <|
                \_ ->
                    Chord.chord PitchClass.c ChordClass.major
                        |> Chord.toPitchClasses
                        |> Expect.equal [ PitchClass.c, PitchClass.e, PitchClass.g ]
            ]
        , describe "allChords"
            [ test "Should return C4 E4 G4 from C major triad in 4th octave" <|
                \_ ->
                    let
                        expected =
                            [ [ Pitch.c0, Pitch.e0, Pitch.g0 ]
                            , [ Pitch.c1, Pitch.e1, Pitch.g1 ]
                            , [ Pitch.c2, Pitch.e2, Pitch.g2 ]
                            , [ Pitch.c3, Pitch.e3, Pitch.g3 ]
                            , [ Pitch.c4, Pitch.e4, Pitch.g4 ]
                            , [ Pitch.c5, Pitch.e5, Pitch.g5 ]
                            , [ Pitch.c6, Pitch.e6, Pitch.g6 ]
                            , [ Pitch.c7, Pitch.e7, Pitch.g7 ]
                            , [ Pitch.c8, Pitch.e8, Pitch.g8 ]
                            ]

                        result =
                            Chord.chord PitchClass.c ChordClass.major
                                |> Chord.allChords
                    in
                    Expect.equal expected result
            ]
        , describe "possibleInversions"
            [ test "Should return three inversions from C major triad" <|
                \_ ->
                    let
                        expected =
                            [ Inversion.none
                            , Inversion.first
                            , Inversion.second
                            ]

                        result =
                            Chord.chord PitchClass.c ChordClass.major
                                |> Chord.possibleInversions
                    in
                    Expect.equal expected result
            ]
        , describe "withInversion"
            [ test "Should return the first inversion from C major triad first inversion" <|
                \_ ->
                    let
                        expected =
                            Ok [ PitchClass.e, PitchClass.g, PitchClass.c ]

                        result =
                            Chord.chord PitchClass.c ChordClass.major
                                |> Chord.withInversion Inversion.first
                                |> Result.map Chord.toPitchClasses
                    in
                    Expect.equal expected result
            , test "Should return E G C for the first inversion of C major triad first inversion" <|
                \_ ->
                    let
                        expected =
                            Ok Inversion.first

                        result =
                            Chord.chord PitchClass.c ChordClass.major
                                |> Chord.withInversion Inversion.first
                                |> Result.map Chord.inversion
                    in
                    Expect.equal expected result
            , test "Should return error for third inversion of C major triad" <|
                \_ ->
                    let
                        expected =
                            Err Chord.InvalidInversion

                        result =
                            Chord.chord PitchClass.c ChordClass.major
                                |> Chord.withInversion Inversion.third
                    in
                    Expect.equal expected result
            , test "Should return E4 G4 C5 for the first inversion of C major triad first inversion" <|
                \_ ->
                    let
                        expected =
                            Ok [ Pitch.e4, Pitch.g4, Pitch.c5 ]

                        result =
                            Chord.chord PitchClass.c ChordClass.major
                                |> Chord.withInversion Inversion.first
                                |> Result.andThen (Chord.toPitchesFromOctave Octave.four)
                    in
                    Expect.equal expected result
            ]
        , describe "inOctave"
            [ test "Should return C4 E4 G4 from C major triad in 4th octave" <|
                \_ ->
                    Chord.chord PitchClass.c ChordClass.major
                        |> Chord.toPitchesFromOctave Octave.four
                        |> Expect.equal (Ok [ Pitch.c4, Pitch.e4, Pitch.g4 ])
            , test "Should return C4 E4 G4 from C dominant thirteenth in 4th octave" <|
                \_ ->
                    Chord.chord PitchClass.c ChordClass.dominantThirteenth
                        |> Chord.toPitchesFromOctave Octave.four
                        |> Expect.equal
                            (Ok
                                [ Pitch.c4
                                , Pitch.e4
                                , Pitch.g4
                                , Pitch.bFlat4
                                , Pitch.d5
                                , Pitch.f5
                                , Pitch.a5
                                ]
                            )
            ]
        ]
