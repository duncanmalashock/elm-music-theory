module Test.Internal.Chord exposing (all)

import Expect
import Internal.Chord as Chord
import Internal.ChordType as ChordType
import Internal.PitchClass as PitchClass
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
                                ChordType.majorSeventh

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
                                ChordType.major

                        bNatural =
                            PitchClass.b

                        result =
                            Chord.containsPitchClass bNatural cMajorSeventh
                    in
                    Expect.equal result False
            ]
        ]
