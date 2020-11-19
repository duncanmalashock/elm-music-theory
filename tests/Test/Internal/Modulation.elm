module Test.Internal.Modulation exposing (..)

import Expect
import Music.Internal.Interval as Interval
import Music.Internal.Key as Key
import Music.Internal.Modulation as Modulation
import Music.Internal.PitchClass as PitchClass
import Test exposing (Test, describe, test)


all : Test
all =
    describe "Modulation Tests"
        [ test "Modulation down a fifth should modulate C to F" <|
            \_ ->
                let
                    expected =
                        Key.f

                    result =
                        Key.c
                            |> Modulation.apply Modulation.normalizeToCircleOfFifths
                                (Modulation.downByFifths 1)
                in
                Expect.equal expected result
        , test "Modulation up a fifth should modulate C to G" <|
            \_ ->
                let
                    expected =
                        Key.g

                    result =
                        Key.c
                            |> Modulation.apply Modulation.normalizeToCircleOfFifths
                                (Modulation.upByFifths 1)
                in
                Expect.equal expected result
        , test "Modulation down two fifths should modulate C to Bb" <|
            \_ ->
                let
                    expected =
                        Key.bFlat

                    result =
                        Key.c
                            |> Modulation.apply Modulation.normalizeToCircleOfFifths
                                (Modulation.downByFifths 2)
                in
                Expect.equal expected result
        , test "Modulation down two fifths should modulate Db to Cb" <|
            \_ ->
                let
                    expected =
                        Key.major PitchClass.cFlat

                    result =
                        Key.dFlat
                            |> Modulation.apply Modulation.normalizeToCircleOfFifths
                                (Modulation.downByFifths 2)
                in
                Expect.equal expected result
        , test "Modulation down 6 fifths should modulate C to Gb" <|
            \_ ->
                let
                    expected =
                        Key.gFlat

                    result =
                        Key.c
                            |> Modulation.apply Modulation.normalizeToCircleOfFifths
                                (Modulation.downByFifths 6)
                in
                Expect.equal expected result
        , test "Modulation up 6 fifths should modulate C to F#" <|
            \_ ->
                let
                    expected =
                        Key.fSharp

                    result =
                        Key.c
                            |> Modulation.apply Modulation.normalizeToCircleOfFifths
                                (Modulation.upByFifths 6)
                in
                Expect.equal expected result
        , test "Modulation to relative key should modulate C to Am" <|
            \_ ->
                let
                    expected =
                        Key.aMinor

                    result =
                        Key.c
                            |> Modulation.apply Modulation.normalizeToCircleOfFifths
                                Modulation.switchToRelativeKey
                in
                Expect.equal expected result
        , test "Modulation to relative key should modulate F#m to A" <|
            \_ ->
                let
                    expected =
                        Key.a

                    result =
                        Key.fSharpMinor
                            |> Modulation.apply Modulation.normalizeToCircleOfFifths
                                Modulation.switchToRelativeKey
                in
                Expect.equal expected result
        , test "Modulation to parallel key should modulate C to Cm" <|
            \_ ->
                let
                    expected =
                        Key.cMinor

                    result =
                        Key.c
                            |> Modulation.apply Modulation.normalizeToCircleOfFifths
                                Modulation.switchToParallelKey
                in
                Expect.equal expected result
        , test "Modulation down a major second should modulate C to Bb" <|
            \_ ->
                let
                    expected =
                        Key.bFlat

                    result =
                        Key.c
                            |> Modulation.apply Modulation.normalizeToCircleOfFifths
                                (Modulation.byInterval
                                    (Interval.majorSecond |> Interval.reverse)
                                )
                in
                Expect.equal expected result
        , test "applyMultiple should perform multiple modulations" <|
            \_ ->
                let
                    expected =
                        Key.bFlat

                    result =
                        Key.c
                            |> Modulation.applyMultiple Modulation.normalizeToCircleOfFifths
                                [ Modulation.upByFifths 1
                                , Modulation.switchToParallelKey
                                , Modulation.switchToRelativeKey
                                ]
                in
                Expect.equal expected result
        , test "Modulation with no normalization should allow the key of D#" <|
            \_ ->
                let
                    expected =
                        Key.major PitchClass.dSharp

                    result =
                        Key.c
                            |> Modulation.apply Modulation.allowTheoreticalKeys
                                (Modulation.byInterval
                                    Interval.augmentedSecond
                                )
                in
                Expect.equal expected result
        , test "Modulation with default normalization should change D# to Eb" <|
            \_ ->
                let
                    expected =
                        Key.major PitchClass.eFlat

                    result =
                        Key.c
                            |> Modulation.apply Modulation.normalizeToCircleOfFifths
                                (Modulation.byInterval
                                    Interval.augmentedSecond
                                )
                in
                Expect.equal expected result
        , test "Modulation with Gb preference should change to Gb" <|
            \_ ->
                let
                    expected =
                        Key.major PitchClass.gFlat

                    result =
                        Key.c
                            |> Modulation.apply Modulation.simplifyAndPreferGFlat
                                (Modulation.upByFifths 6)
                in
                Expect.equal expected result
        , test "Modulation with F# preference should change to F#" <|
            \_ ->
                let
                    expected =
                        Key.major PitchClass.fSharp

                    result =
                        Key.c
                            |> Modulation.apply Modulation.simplifyAndPreferFSharp
                                (Modulation.downByFifths 6)
                in
                Expect.equal expected result
        ]
