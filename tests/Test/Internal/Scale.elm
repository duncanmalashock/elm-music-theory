module Test.Internal.Scale exposing (all)

import Expect
import Internal.Letter exposing (Letter(..))
import Internal.Pitch as Pitch
import Internal.PitchClass as PitchClass exposing (natural, pitchClass)
import Internal.Scale as Scale
import Internal.ScaleClass as ScaleClass
import Test exposing (Test, describe, test)


all : Test
all =
    describe "Scale Tests"
        [ test "C major scale should have C root" <|
            \_ ->
                Scale.scale (pitchClass C natural) ScaleClass.ionian
                    |> Scale.root
                    |> Expect.equal (pitchClass C natural)
        , test "C ionian should have correct pitch classes" <|
            \_ ->
                let
                    pitchClassesInCIonian =
                        [ pitchClass C natural
                        , pitchClass D natural
                        , pitchClass E natural
                        , pitchClass F natural
                        , pitchClass G natural
                        , pitchClass A natural
                        , pitchClass B natural
                        ]
                in
                Scale.scale (pitchClass C natural) ScaleClass.ionian
                    |> Scale.toList
                    |> Expect.equal pitchClassesInCIonian
        , test "D dorian should have correct pitch classes" <|
            \_ ->
                let
                    pitchClassesInCDorian =
                        [ pitchClass D natural
                        , pitchClass E natural
                        , pitchClass F natural
                        , pitchClass G natural
                        , pitchClass A natural
                        , pitchClass B natural
                        , pitchClass C natural
                        ]
                in
                Scale.scale (pitchClass D natural) ScaleClass.dorian
                    |> Scale.toList
                    |> Expect.equal pitchClassesInCDorian
        , test "E phrygian should have correct pitch classes" <|
            \_ ->
                let
                    pitchClassesInCPhrygian =
                        [ pitchClass E natural
                        , pitchClass F natural
                        , pitchClass G natural
                        , pitchClass A natural
                        , pitchClass B natural
                        , pitchClass C natural
                        , pitchClass D natural
                        ]
                in
                Scale.scale (pitchClass E natural) ScaleClass.phrygian
                    |> Scale.toList
                    |> Expect.equal pitchClassesInCPhrygian
        , test "F lydian should have correct pitch classes" <|
            \_ ->
                let
                    pitchClassesInFLydian =
                        [ pitchClass F natural
                        , pitchClass G natural
                        , pitchClass A natural
                        , pitchClass B natural
                        , pitchClass C natural
                        , pitchClass D natural
                        , pitchClass E natural
                        ]
                in
                Scale.scale (pitchClass F natural) ScaleClass.lydian
                    |> Scale.toList
                    |> Expect.equal pitchClassesInFLydian
        , test "G mixolydian should have correct pitch classes" <|
            \_ ->
                let
                    pitchClassesInGMixolydian =
                        [ pitchClass G natural
                        , pitchClass A natural
                        , pitchClass B natural
                        , pitchClass C natural
                        , pitchClass D natural
                        , pitchClass E natural
                        , pitchClass F natural
                        ]
                in
                Scale.scale (pitchClass G natural) ScaleClass.mixolydian
                    |> Scale.toList
                    |> Expect.equal pitchClassesInGMixolydian
        , test "A aeolian should have correct pitch classes" <|
            \_ ->
                let
                    pitchClassesInAAeolian =
                        [ pitchClass A natural
                        , pitchClass B natural
                        , pitchClass C natural
                        , pitchClass D natural
                        , pitchClass E natural
                        , pitchClass F natural
                        , pitchClass G natural
                        ]
                in
                Scale.scale (pitchClass A natural) ScaleClass.aeolian
                    |> Scale.toList
                    |> Expect.equal pitchClassesInAAeolian
        , test "B locrian should have correct pitch classes" <|
            \_ ->
                let
                    pitchClassesInBLocrian =
                        [ pitchClass B natural
                        , pitchClass C natural
                        , pitchClass D natural
                        , pitchClass E natural
                        , pitchClass F natural
                        , pitchClass G natural
                        , pitchClass A natural
                        ]
                in
                Scale.scale (pitchClass B natural) ScaleClass.locrian
                    |> Scale.toList
                    |> Expect.equal pitchClassesInBLocrian
        , describe "degree"
            [ test "Fourth degree of C major is F" <|
                \_ ->
                    Scale.scale PitchClass.c ScaleClass.ionian
                        |> Scale.degree 4
                        |> Expect.equal PitchClass.f
            ]
        , describe "toListThroughAllOctaves"
            [ test "F lydian should contain pitches through all octaves" <|
                \_ ->
                    let
                        expected =
                            [ Pitch.f0
                            , Pitch.g0
                            , Pitch.a0
                            , Pitch.b0
                            , Pitch.c1
                            , Pitch.d1
                            , Pitch.e1
                            , Pitch.f1
                            , Pitch.g1
                            , Pitch.a1
                            , Pitch.b1
                            , Pitch.c2
                            , Pitch.d2
                            , Pitch.e2
                            , Pitch.f2
                            , Pitch.g2
                            , Pitch.a2
                            , Pitch.b2
                            , Pitch.c3
                            , Pitch.d3
                            , Pitch.e3
                            , Pitch.f3
                            , Pitch.g3
                            , Pitch.a3
                            , Pitch.b3
                            , Pitch.c4
                            , Pitch.d4
                            , Pitch.e4
                            , Pitch.f4
                            , Pitch.g4
                            , Pitch.a4
                            , Pitch.b4
                            , Pitch.c5
                            , Pitch.d5
                            , Pitch.e5
                            , Pitch.f5
                            , Pitch.g5
                            , Pitch.a5
                            , Pitch.b5
                            , Pitch.c6
                            , Pitch.d6
                            , Pitch.e6
                            , Pitch.f6
                            , Pitch.g6
                            , Pitch.a6
                            , Pitch.b6
                            , Pitch.c7
                            , Pitch.d7
                            , Pitch.e7
                            , Pitch.f7
                            , Pitch.g7
                            , Pitch.a7
                            , Pitch.b7
                            , Pitch.c8
                            , Pitch.d8
                            , Pitch.e8
                            ]

                        result =
                            Scale.toListThroughAllOctaves
                                (Scale.scale PitchClass.f ScaleClass.lydian)
                    in
                    Expect.equal expected result
            ]
        ]
