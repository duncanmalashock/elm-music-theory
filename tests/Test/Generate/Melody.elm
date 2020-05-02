module Test.Generate.Melody exposing (all)

import Expect
import MusicTheory.Generate.Melody as Melody
import MusicTheory.Pitch as Pitch
import MusicTheory.PitchClass as PitchClass
import MusicTheory.Scale as Scale
import MusicTheory.ScaleClass as ScaleClass
import Test exposing (..)


all : Test
all =
    describe "all"
        [ describe "init"
            [ test "should create a scale stepper" <|
                \_ ->
                    let
                        expected =
                            ( 56, Pitch.c4 )

                        result =
                            Melody.init (Scale.scale PitchClass.c ScaleClass.ionian) Pitch.c4
                                |> (\stepper ->
                                        ( Melody.length stepper, Melody.current stepper )
                                   )
                    in
                    Expect.equal expected result
            ]
        , describe "generate"
            [ test "should generate a list of pitches from the scale" <|
                \_ ->
                    let
                        expected =
                            [ Pitch.c4
                            , Pitch.eFlat4
                            , Pitch.d4
                            , Pitch.f4
                            , Pitch.eFlat4
                            , Pitch.g4
                            ]

                        result =
                            Melody.init (Scale.scale PitchClass.c ScaleClass.aeolian) Pitch.c4
                                |> Melody.generate [ 0, 2, -1, 2, -1, 2 ]
                    in
                    Expect.equal expected result
            ]
        ]
