module ScaleTests exposing (all)

import Expect
import MusicTheory.Letter exposing (Letter(..))
import MusicTheory.PitchClass exposing (doubleFlat, flat, natural, pitchClass, sharp)
import MusicTheory.Scale as Scale
import MusicTheory.ScaleClass as ScaleClass
import Test exposing (Test, describe, test)


all : Test
all =
    describe "Scale Tests"
        [ test "C major scale should have C root" <|
            \_ ->
                Scale.scale (pitchClass C natural) ScaleClass.major
                    |> Scale.root
                    |> Expect.equal (pitchClass C natural)
        , test "C major" <|
            \_ ->
                let
                    pitchClassesInCMajor =
                        [ pitchClass C natural
                        , pitchClass D natural
                        , pitchClass E natural
                        , pitchClass F natural
                        , pitchClass G natural
                        , pitchClass A natural
                        , pitchClass B natural
                        ]
                in
                Scale.scale (pitchClass C natural) ScaleClass.major
                    |> Scale.toList
                    |> Expect.equal pitchClassesInCMajor
        , test "A minor" <|
            \_ ->
                let
                    pitchClassesInAMinor =
                        [ pitchClass A natural
                        , pitchClass B natural
                        , pitchClass C natural
                        , pitchClass D natural
                        , pitchClass E natural
                        , pitchClass F natural
                        , pitchClass G natural
                        ]
                in
                Scale.scale (pitchClass A natural) ScaleClass.minor
                    |> Scale.toList
                    |> Expect.equal pitchClassesInAMinor
        , test "C major pentatonic" <|
            \_ ->
                let
                    pitchClassesInCMajorPentatonic =
                        [ pitchClass C natural
                        , pitchClass D natural
                        , pitchClass E natural
                        , pitchClass G natural
                        , pitchClass A natural
                        ]
                in
                Scale.scale (pitchClass C natural) ScaleClass.majorPentatonic
                    |> Scale.toList
                    |> Expect.equal pitchClassesInCMajorPentatonic
        , test "A minor pentatonic" <|
            \_ ->
                let
                    pitchClassesInAMinorPentatonic =
                        [ pitchClass A natural
                        , pitchClass C natural
                        , pitchClass D natural
                        , pitchClass E natural
                        , pitchClass G natural
                        ]
                in
                Scale.scale (pitchClass A natural) ScaleClass.minorPentatonic
                    |> Scale.toList
                    |> Expect.equal pitchClassesInAMinorPentatonic
        , test "C whole tone" <|
            \_ ->
                let
                    pitchClassesInCWholeTone =
                        [ pitchClass C natural
                        , pitchClass D natural
                        , pitchClass E natural
                        , pitchClass F sharp
                        , pitchClass G sharp
                        , pitchClass A sharp
                        ]
                in
                Scale.scale (pitchClass C natural) ScaleClass.wholeTone
                    |> Scale.toList
                    |> Expect.equal pitchClassesInCWholeTone
        , test "C diminished half-whole" <|
            \_ ->
                let
                    pitchClassesInCDiminishedHalfWhole =
                        [ pitchClass C natural
                        , pitchClass D flat
                        , pitchClass E flat
                        , pitchClass F flat
                        , pitchClass G flat
                        , pitchClass A doubleFlat
                        , pitchClass B doubleFlat
                        , pitchClass C doubleFlat
                        ]
                in
                Scale.scale (pitchClass C natural) ScaleClass.diminishedHalfToneWholeTone
                    |> Scale.toList
                    |> Expect.equal pitchClassesInCDiminishedHalfWhole
        , test "C diminished whole-half" <|
            \_ ->
                let
                    pitchClassesInCDiminishedWholeHalf =
                        [ pitchClass C natural
                        , pitchClass D natural
                        , pitchClass E flat
                        , pitchClass F natural
                        , pitchClass G flat
                        , pitchClass A flat
                        , pitchClass B doubleFlat
                        , pitchClass C flat
                        ]
                in
                Scale.scale (pitchClass C natural) ScaleClass.diminishedWholeToneHalfTone
                    |> Scale.toList
                    |> Expect.equal pitchClassesInCDiminishedWholeHalf
        , test "C minor major sixth pentatonic" <|
            \_ ->
                let
                    expected =
                        [ pitchClass C natural
                        , pitchClass E flat
                        , pitchClass F natural
                        , pitchClass G natural
                        , pitchClass A natural
                        ]
                in
                Scale.scale (pitchClass C natural) ScaleClass.minor6Pentatonic
                    |> Scale.toList
                    |> Expect.equal expected
        , test "C major minor sixth pentatonic" <|
            \_ ->
                let
                    expected =
                        [ pitchClass C natural
                        , pitchClass D natural
                        , pitchClass E natural
                        , pitchClass G natural
                        , pitchClass A flat
                        ]
                in
                Scale.scale (pitchClass C natural) ScaleClass.majorFlat6Pentatonic
                    |> Scale.toList
                    |> Expect.equal expected
        , test "C minor 7 diminished fifth pentatonic" <|
            \_ ->
                let
                    expected =
                        [ pitchClass C natural
                        , pitchClass E flat
                        , pitchClass F natural
                        , pitchClass G flat
                        , pitchClass B flat
                        ]
                in
                Scale.scale (pitchClass C natural) ScaleClass.minorFlat5Pentatonic
                    |> Scale.toList
                    |> Expect.equal expected
        , test "C major minor 2 pentatonic" <|
            \_ ->
                let
                    expected =
                        [ pitchClass C natural
                        , pitchClass D flat
                        , pitchClass E natural
                        , pitchClass G natural
                        , pitchClass A natural
                        ]
                in
                Scale.scale (pitchClass C natural) ScaleClass.majorFlat2Pentatonic
                    |> Scale.toList
                    |> Expect.equal expected
        ]
