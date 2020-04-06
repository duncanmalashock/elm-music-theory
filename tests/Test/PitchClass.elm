module Test.PitchClass exposing (all)

import Expect
import MusicTheory.Interval as Interval
import MusicTheory.Letter exposing (Letter(..))
import MusicTheory.PitchClass as PitchClass
import Test exposing (Test, describe, test)


all : Test
all =
    describe "Pitch Class Tests"
        [ describe "semitones"
            [ test "semitones of C should be 0" <|
                \_ ->
                    let
                        result =
                            PitchClass.semitones PitchClass.c

                        expected =
                            0
                    in
                    Expect.equal result expected
            , test "semitones of C double flat should be -2" <|
                \_ ->
                    let
                        result =
                            PitchClass.semitones PitchClass.cDoubleFlat

                        expected =
                            -2
                    in
                    Expect.equal result expected
            , test "semitones of B double sharp should be 13" <|
                \_ ->
                    let
                        result =
                            PitchClass.semitones PitchClass.bDoubleSharp

                        expected =
                            13
                    in
                    Expect.equal result expected
            ]
        , describe "transposeUp"
            [ test "C tranposed up an major sixth should be A" <|
                \_ ->
                    let
                        result =
                            PitchClass.transpose Interval.majorSixth PitchClass.c

                        expected =
                            PitchClass.a
                    in
                    Expect.equal result expected
            ]
        , describe "transposeDown"
            [ test "C tranposed down an augmented fourth should be G flat" <|
                \_ ->
                    let
                        result =
                            PitchClass.transpose
                                (Interval.augmentedFourth
                                    |> Interval.reverse
                                )
                                PitchClass.c

                        expected =
                            PitchClass.gFlat
                    in
                    Expect.equal result expected
            ]
        , describe "areEnharmonicEquivalents"
            [ test "F and G double flat should be equivalent" <|
                \_ ->
                    let
                        result =
                            PitchClass.areEnharmonicEquivalents
                                PitchClass.f
                                PitchClass.gDoubleFlat

                        expected =
                            True
                    in
                    Expect.equal result expected
            , test "E and F should not be equivalent" <|
                \_ ->
                    let
                        result =
                            PitchClass.areEnharmonicEquivalents
                                PitchClass.e
                                PitchClass.f

                        expected =
                            False
                    in
                    Expect.equal result expected
            ]
        ]
