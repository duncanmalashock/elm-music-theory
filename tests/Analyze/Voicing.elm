module Analyze.Voicing exposing (all)

import Expect
import MusicTheory.Analyze.Voicing as AnalyzeVoicing
import MusicTheory.Pitch as Pitch
import Test exposing (..)


all : Test
all =
    describe "all"
        [ describe "diffFourParts"
            [ test "First inversion of C major seventh should be 12 semitones different from root position" <|
                \_ ->
                    let
                        cMaj7root =
                            { voiceOne = Pitch.c4
                            , voiceTwo = Pitch.e4
                            , voiceThree = Pitch.g4
                            , voiceFour = Pitch.b4
                            }

                        cMaj7FirstInv =
                            { voiceOne = Pitch.e4
                            , voiceTwo = Pitch.g4
                            , voiceThree = Pitch.b4
                            , voiceFour = Pitch.c5
                            }

                        result =
                            AnalyzeVoicing.diffFourParts cMaj7root cMaj7FirstInv

                        expected =
                            12
                    in
                    Expect.equal result expected
            , test "C major six should be 2 semitones different from C major seven" <|
                \_ ->
                    let
                        cMaj7 =
                            { voiceOne = Pitch.c4
                            , voiceTwo = Pitch.e4
                            , voiceThree = Pitch.g4
                            , voiceFour = Pitch.b4
                            }

                        cMaj6 =
                            { voiceOne = Pitch.c4
                            , voiceTwo = Pitch.e4
                            , voiceThree = Pitch.g4
                            , voiceFour = Pitch.a4
                            }

                        result =
                            AnalyzeVoicing.diffFourParts cMaj6 cMaj7

                        expected =
                            2
                    in
                    Expect.equal result expected
            ]
        ]
