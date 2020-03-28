module Test.Generate.Voicing exposing (all)

import Expect
import MusicTheory.Analyze.Chord as AnalyzeChord
import MusicTheory.Chord as Chord
import MusicTheory.ChordClass as ChordClass
import MusicTheory.Generate.Voicing as GenerateVoicing
import MusicTheory.Pitch as Pitch
import MusicTheory.PitchClass as PitchClass
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

                        availablePitchClasses =
                            AnalyzeChord.availablePitchClassesFor
                                (Chord.chord PitchClass.c ChordClass.majorSix)
                                |> Result.mapError GenerateVoicing.AnalyzeChordError

                        result =
                            Result.andThen
                                (GenerateVoicing.fourWayClose Pitch.e5)
                                availablePitchClasses
                    in
                    Expect.equal result expected
            ]
        ]
