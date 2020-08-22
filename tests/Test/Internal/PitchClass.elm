module Test.Internal.PitchClass exposing (all)

import Expect
import Internal.Interval as Interval
import Internal.PitchClass as PitchClass
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
                    Expect.equal expected result
            , test "semitones of C double flat should be -2" <|
                \_ ->
                    let
                        result =
                            PitchClass.semitones PitchClass.cDoubleFlat

                        expected =
                            -2
                    in
                    Expect.equal expected result
            , test "semitones of B double sharp should be 13" <|
                \_ ->
                    let
                        result =
                            PitchClass.semitones PitchClass.bDoubleSharp

                        expected =
                            13
                    in
                    Expect.equal expected result
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
                    Expect.equal expected result
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
                    Expect.equal expected result
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
                    Expect.equal expected result
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
                    Expect.equal expected result
            ]
        ]
