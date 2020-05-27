module Test.Generate.JazzVoicing exposing (all)

import Expect
import MusicTheory.Analyze.JazzChord as AnalyzeChord
import MusicTheory.Chord as Chord
import MusicTheory.ChordClass as ChordClass
import MusicTheory.Generate.JazzVoicing as GenerateVoicing
import MusicTheory.Interval as Interval exposing (IntervalNumber(..))
import MusicTheory.Octave as Octave
import MusicTheory.Pitch as Pitch
import MusicTheory.PitchClass as PitchClass
import MusicTheory.Voicing as Voicing
import Result.Extra
import Test exposing (..)


all : Test
all =
    describe "all"
        [ describe "fourWayClose"
            [ test "four-way close voicing of Cmaj6 should include correct chord tones" <|
                \_ ->
                    let
                        expected =
                            Ok
                                { voiceOne = Pitch.e5
                                , voiceTwo = Pitch.c5
                                , voiceThree = Pitch.a4
                                , voiceFour = Pitch.g4
                                }

                        chord =
                            Chord.chord PitchClass.c ChordClass.majorSix

                        availables =
                            AnalyzeChord.availables chord
                                |> Result.mapError GenerateVoicing.AnalyzeChordError

                        result =
                            Result.andThen
                                (GenerateVoicing.fourWayClose Interval.majorThird)
                                availables
                                |> Result.map (Voicing.fourPart chord Octave.five)
                                |> Result.map Voicing.toPitchesFourPart
                    in
                    Expect.equal expected result
            ]
        , describe "fourWayDrop2"
            [ test "four-way drop-2 voicing of Cmaj6 should include correct chord tones" <|
                \_ ->
                    let
                        expected =
                            Ok
                                { voiceOne = Pitch.e5
                                , voiceTwo = Pitch.a4
                                , voiceThree = Pitch.g4
                                , voiceFour = Pitch.c4
                                }

                        chord =
                            Chord.chord PitchClass.c ChordClass.majorSix

                        availables =
                            AnalyzeChord.availables chord
                                |> Result.mapError GenerateVoicing.AnalyzeChordError

                        result =
                            Result.andThen
                                (GenerateVoicing.fourWayClose Interval.majorThird)
                                availables
                                |> Result.map2
                                    GenerateVoicing.fourWayDrop2
                                    availables
                                |> Result.Extra.join
                                |> Result.map (Voicing.fourPart chord Octave.five)
                                |> Result.map Voicing.toPitchesFourPart
                    in
                    Expect.equal expected result
            ]
        , describe "fourWayDrop3"
            [ test "four-way drop-3 voicing of Cmaj6 should include correct chord tones" <|
                \_ ->
                    let
                        expected =
                            Ok
                                { voiceOne = Pitch.e5
                                , voiceTwo = Pitch.c5
                                , voiceThree = Pitch.g4
                                , voiceFour = Pitch.a3
                                }

                        chord =
                            Chord.chord PitchClass.c ChordClass.majorSix

                        availables =
                            AnalyzeChord.availables
                                chord
                                |> Result.mapError GenerateVoicing.AnalyzeChordError

                        result =
                            Result.andThen
                                (GenerateVoicing.fourWayClose Interval.majorThird)
                                availables
                                |> Result.map2
                                    GenerateVoicing.fourWayDrop3
                                    availables
                                |> Result.Extra.join
                                |> Result.map (Voicing.fourPart chord Octave.five)
                                |> Result.map Voicing.toPitchesFourPart
                    in
                    Expect.equal expected result
            ]
        , describe "fourWayDrop2and4"
            [ test "four-way drop-2-and-4 voicing of Cmaj6 should include correct chord tones" <|
                \_ ->
                    let
                        expected =
                            Ok
                                { voiceOne = Pitch.e5
                                , voiceTwo = Pitch.a4
                                , voiceThree = Pitch.c4
                                , voiceFour = Pitch.g3
                                }

                        chord =
                            Chord.chord PitchClass.c ChordClass.majorSix

                        availables =
                            AnalyzeChord.availables chord
                                |> Result.mapError GenerateVoicing.AnalyzeChordError

                        result =
                            Result.andThen
                                (GenerateVoicing.fourWayClose Interval.majorThird)
                                availables
                                |> Result.map2
                                    GenerateVoicing.fourWayDrop2and4
                                    availables
                                |> Result.Extra.join
                                |> Result.map (Voicing.fourPart chord Octave.five)
                                |> Result.map Voicing.toPitchesFourPart
                    in
                    Expect.equal expected result
            ]
        , describe "fourWayCloseDoubleLead"
            [ test "four-way close double-lead voicing of Cmaj6 should include correct chord tones" <|
                \_ ->
                    let
                        expected =
                            Ok
                                { voiceOne = Pitch.e5
                                , voiceTwo = Pitch.c5
                                , voiceThree = Pitch.a4
                                , voiceFour = Pitch.g4
                                , voiceFive = Pitch.e4
                                }

                        availables =
                            AnalyzeChord.availables
                                (Chord.chord PitchClass.c ChordClass.majorSix)
                                |> Result.mapError GenerateVoicing.AnalyzeChordError

                        result =
                            Result.andThen
                                (GenerateVoicing.fourWayCloseDoubleLead Interval.majorThird)
                                availables
                                |> Result.map (Voicing.fivePart Pitch.c5)
                                |> Result.map Voicing.toPitchesFivePart
                    in
                    Expect.equal expected result
            ]
        , describe "fourWaySpread"
            [ test "four-way spread voicing of Cmaj6 should include correct chord tones" <|
                \_ ->
                    let
                        expected =
                            Ok
                                { voiceOne = Pitch.d4
                                , voiceTwo = Pitch.a3
                                , voiceThree = Pitch.e3
                                , voiceFour = Pitch.c3
                                }

                        chord =
                            Chord.chord PitchClass.c ChordClass.majorSix

                        availables =
                            AnalyzeChord.availables chord
                                |> Result.mapError GenerateVoicing.AnalyzeChordError

                        result =
                            Result.andThen
                                (GenerateVoicing.fourWaySpread Interval.perfectUnison)
                                availables
                                |> Result.map (Voicing.fourPart chord Octave.three)
                                |> Result.map Voicing.toPitchesFourPart
                    in
                    Expect.equal expected result
            ]
        , describe "fiveWaySpread"
            [ test "five-way spread voicing of C7b9b13 should include correct chord tones" <|
                \_ ->
                    let
                        expected =
                            Ok
                                { voiceOne = Pitch.aFlat4
                                , voiceTwo = Pitch.dFlat4
                                , voiceThree = Pitch.bFlat3
                                , voiceFour = Pitch.e3
                                , voiceFive = Pitch.c3
                                }

                        availables =
                            AnalyzeChord.availables
                                (Chord.chord PitchClass.c ChordClass.dominantSeventhFlatNineFlatThirteen)
                                |> Result.mapError GenerateVoicing.AnalyzeChordError

                        result =
                            availables
                                |> Result.andThen
                                    (GenerateVoicing.fiveWaySpread Interval.perfectUnison)
                                |> Result.map (Voicing.fivePart Pitch.c3)
                                |> Result.map Voicing.toPitchesFivePart
                    in
                    Expect.equal expected result
            ]
        , describe "substituteDoubleLead"
            [ test "substitute double lead voicing of Cm7 should include correct chord tones" <|
                \_ ->
                    let
                        expected =
                            Ok
                                { voiceOne = Pitch.d5
                                , voiceTwo = Pitch.bFlat4
                                , voiceThree = Pitch.g4
                                , voiceFour = Pitch.eFlat4
                                , voiceFive = Pitch.c4
                                }

                        availables =
                            AnalyzeChord.availables
                                (Chord.chord PitchClass.c ChordClass.minorSeventh)
                                |> Result.mapError GenerateVoicing.AnalyzeChordError

                        result =
                            Result.andThen
                                (GenerateVoicing.substituteDoubleLead Interval.majorNinth)
                                availables
                                |> Result.map (Voicing.fivePart Pitch.c4)
                                |> Result.map Voicing.toPitchesFivePart
                    in
                    Expect.equal expected result
            ]
        ]
