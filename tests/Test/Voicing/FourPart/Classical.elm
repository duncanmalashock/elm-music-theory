module Test.Voicing.FourPart.Classical exposing (..)

import Expect
import MusicTheory.Chord as Chord
import MusicTheory.ChordClass as ChordClass
import MusicTheory.Pitch as Pitch
import MusicTheory.PitchClass as PitchClass
import MusicTheory.Voicing as Voicing
import MusicTheory.Voicing.FourPart.Classical as Classical
import Test exposing (..)
import Test.Util


all : Test
all =
    describe "all"
        [ describe "rootPosition"
            [ test "should generate voicings of the chord" <|
                \_ ->
                    let
                        resultIsNonEmpty =
                            Classical.rootPosition
                                { ranges =
                                    Classical.satbRanges
                                , chord =
                                    Chord.chord
                                        PitchClass.c
                                        ChordClass.dominantSeventh
                                }
                                |> List.map voicingToString
                                |> List.isEmpty
                                |> not
                    in
                    Expect.true "List of generated voicings was empty." resultIsNonEmpty
            , test "all voicings should have root in the fourth voice" <|
                \_ ->
                    let
                        results =
                            Classical.rootPosition
                                { ranges =
                                    Classical.satbRanges
                                , chord =
                                    Chord.chord
                                        PitchClass.c
                                        ChordClass.dominantSeventh
                                }
                    in
                    Test.Util.expectAllInList (checkVoice .voiceFour PitchClass.c) results
            , test "all voicings of a triad should include the third" <|
                \_ ->
                    let
                        results =
                            Classical.rootPosition
                                { ranges =
                                    Classical.satbRanges
                                , chord =
                                    Chord.chord
                                        PitchClass.c
                                        ChordClass.major
                                }
                    in
                    Test.Util.expectAllInList (checkIncludesPitchClass PitchClass.e) results
            , test "all voicings of a seventh chord should include the seventh" <|
                \_ ->
                    let
                        results =
                            Classical.rootPosition
                                { ranges =
                                    Classical.satbRanges
                                , chord =
                                    Chord.chord
                                        PitchClass.c
                                        ChordClass.dominantSeventh
                                }
                    in
                    Test.Util.expectAllInList (checkIncludesPitchClass PitchClass.bFlat) results
            ]
        ]


checkVoice :
    (Voicing.PitchesFourPart -> Pitch.Pitch)
    -> PitchClass.PitchClass
    -> Voicing.FourPartVoicing
    -> Maybe String
checkVoice getter expectedPitchClass voicing =
    voicing
        |> Voicing.toPitchesFourPart
        |> getter
        |> (\pitch ->
                if Pitch.pitchClass pitch == expectedPitchClass then
                    Nothing

                else
                    Just <|
                        "Expected pitch class in voicing "
                            ++ voicingToString voicing
                            ++ " to be "
                            ++ PitchClass.toString expectedPitchClass
                            ++ ", but got "
                            ++ (PitchClass.toString <|
                                    Pitch.pitchClass pitch
                               )
                            ++ ".\n"
           )


checkIncludesPitchClass :
    PitchClass.PitchClass
    -> Voicing.FourPartVoicing
    -> Maybe String
checkIncludesPitchClass expectedPitchClass voicing =
    voicing
        |> Voicing.toPitchesFourPart
        |> (\{ voiceOne, voiceTwo, voiceThree, voiceFour } ->
                if
                    List.member expectedPitchClass
                        [ Pitch.pitchClass voiceOne
                        , Pitch.pitchClass voiceTwo
                        , Pitch.pitchClass voiceThree
                        , Pitch.pitchClass voiceFour
                        ]
                then
                    Nothing

                else
                    Just <|
                        "Expected voicing "
                            ++ voicingToString voicing
                            ++ " to contain "
                            ++ PitchClass.toString expectedPitchClass
                            ++ ".\n"
           )


voicingToString : Voicing.FourPartVoicing -> String
voicingToString theVoicing =
    theVoicing
        |> Voicing.toPitchesFourPart
        |> (\{ voiceOne, voiceTwo, voiceThree, voiceFour } ->
                String.join " "
                    [ Pitch.toString voiceFour
                    , Pitch.toString voiceThree
                    , Pitch.toString voiceTwo
                    , Pitch.toString voiceOne
                    ]
           )
