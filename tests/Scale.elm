module Scale exposing (all)

import Expect
import MusicTheory.Letter exposing (Letter(..))
import MusicTheory.PitchClass exposing (natural, pitchClass)
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
        , test "C ionian" <|
            \_ ->
                let
                    pitchClassesInCAeolian =
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
                    |> Expect.equal pitchClassesInCAeolian
        , test "D dorian" <|
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
        , test "E phrygian" <|
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
        , test "F lydian" <|
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
        , test "G mixolydian" <|
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
        , test "A aeolian" <|
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
        , test "B locrian" <|
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
        ]
