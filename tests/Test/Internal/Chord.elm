module Test.Internal.Chord exposing (all)

import Expect
import MusicTheory.Internal.Chord as Chord
import MusicTheory.Internal.ChordClass as ChordClass
import MusicTheory.Internal.PitchClass as PitchClass
import Test exposing (Test, describe, test)


all : Test
all =
    describe "Chord Tests"
        [ describe "containsPitchClass"
            [ test "C major seventh should contain B natural" <|
                \_ ->
                    let
                        cMajorSeventh =
                            Chord.chord
                                PitchClass.c
                                ChordClass.majorSeventh

                        bNatural =
                            PitchClass.b

                        result =
                            Chord.containsPitchClass bNatural cMajorSeventh
                    in
                    Expect.equal result True
            , test "C major should not contain B natural" <|
                \_ ->
                    let
                        cMajorSeventh =
                            Chord.chord
                                PitchClass.c
                                ChordClass.major

                        bNatural =
                            PitchClass.b

                        result =
                            Chord.containsPitchClass bNatural cMajorSeventh
                    in
                    Expect.equal result False
            ]
        ]
