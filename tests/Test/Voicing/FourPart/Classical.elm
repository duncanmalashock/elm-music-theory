module Test.Voicing.FourPart.Classical exposing (..)

import Expect
import MusicTheory.Chord as Chord
import MusicTheory.ChordClass as ChordClass
import MusicTheory.Interval as Interval
import MusicTheory.Pitch as Pitch
import MusicTheory.PitchClass as PitchClass
import MusicTheory.Voicing as Voicing
import MusicTheory.Voicing.FourPart as FourPart
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
                                |> List.map Test.Util.voicingToString
                                |> List.isEmpty
                                |> not
                    in
                    resultIsNonEmpty
                        |> Expect.true "List of generated voicings was empty."
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
                    results
                        |> Test.Util.expectAllInList
                            (checkVoice .voiceFour PitchClass.c)
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
                    results
                        |> Test.Util.expectAllInList
                            (checkIncludesPitchClass PitchClass.e)
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
                    results
                        |> Test.Util.expectAllInList
                            (checkIncludesPitchClass PitchClass.bFlat)
            ]
        , describe "firstInversion"
            [ test "should generate voicings of the chord" <|
                \_ ->
                    let
                        resultIsNonEmpty =
                            Classical.firstInversion
                                { ranges =
                                    Classical.satbRanges
                                , chord =
                                    Chord.chord
                                        PitchClass.c
                                        ChordClass.dominantSeventh
                                }
                                |> List.map Test.Util.voicingToString
                                |> List.isEmpty
                                |> not
                    in
                    resultIsNonEmpty
                        |> Expect.true "List of generated voicings was empty."
            , test "all voicings should have the third in the fourth voice" <|
                \_ ->
                    let
                        results =
                            Classical.firstInversion
                                { ranges =
                                    Classical.satbRanges
                                , chord =
                                    Chord.chord
                                        PitchClass.c
                                        ChordClass.dominantSeventh
                                }
                    in
                    results
                        |> Test.Util.expectAllInList
                            (checkVoice .voiceFour PitchClass.e)
            , test "voicings of triads should not double the third" <|
                \_ ->
                    let
                        results =
                            Classical.firstInversion
                                { ranges =
                                    Classical.satbRanges
                                , chord =
                                    Chord.chord
                                        PitchClass.c
                                        ChordClass.major
                                }
                    in
                    results
                        |> Test.Util.expectAllInList
                            (checkDoesNotDoublePitchClass PitchClass.e)
            , test "voicings of seventh chords should not double the third" <|
                \_ ->
                    let
                        results =
                            Classical.firstInversion
                                { ranges =
                                    Classical.satbRanges
                                , chord =
                                    Chord.chord
                                        PitchClass.c
                                        ChordClass.dominantSeventh
                                }
                    in
                    results
                        |> Test.Util.expectAllInList
                            (checkDoesNotDoublePitchClass PitchClass.e)
            , test "voicings of seventh chords should not double the fifth" <|
                \_ ->
                    let
                        results =
                            Classical.firstInversion
                                { ranges =
                                    Classical.satbRanges
                                , chord =
                                    Chord.chord
                                        PitchClass.c
                                        ChordClass.dominantSeventh
                                }
                    in
                    results
                        |> Test.Util.expectAllInList
                            (checkDoesNotDoublePitchClass PitchClass.g)
            ]
        , describe "secondInversion"
            [ test "should generate voicings of the chord" <|
                \_ ->
                    let
                        resultIsNonEmpty =
                            Classical.secondInversion
                                { ranges =
                                    Classical.satbRanges
                                , chord =
                                    Chord.chord
                                        PitchClass.c
                                        ChordClass.dominantSeventh
                                }
                                |> List.map Test.Util.voicingToString
                                |> List.isEmpty
                                |> not
                    in
                    resultIsNonEmpty
                        |> Expect.true "List of generated voicings was empty."
            , test "all voicings should have the fifth in the fourth voice" <|
                \_ ->
                    let
                        results =
                            Classical.secondInversion
                                { ranges =
                                    Classical.satbRanges
                                , chord =
                                    Chord.chord
                                        PitchClass.c
                                        ChordClass.dominantSeventh
                                }
                    in
                    results
                        |> Test.Util.expectAllInList
                            (checkVoice .voiceFour PitchClass.g)
            , test "voicings of triads should not double the root" <|
                \_ ->
                    let
                        results =
                            Classical.secondInversion
                                { ranges =
                                    Classical.satbRanges
                                , chord =
                                    Chord.chord
                                        PitchClass.c
                                        ChordClass.major
                                }
                    in
                    results
                        |> Test.Util.expectAllInList
                            (checkDoesNotDoublePitchClass PitchClass.c)
            , test "voicings of seventh chords should not double the seventh" <|
                \_ ->
                    let
                        results =
                            Classical.secondInversion
                                { ranges =
                                    Classical.satbRanges
                                , chord =
                                    Chord.chord
                                        PitchClass.c
                                        ChordClass.dominantSeventh
                                }
                    in
                    results
                        |> Test.Util.expectAllInList
                            (checkDoesNotDoublePitchClass PitchClass.bFlat)
            ]
        , describe "thirdInversion"
            [ test "should generate voicings of the chord" <|
                \_ ->
                    let
                        resultIsNonEmpty =
                            Classical.thirdInversion
                                { ranges =
                                    Classical.satbRanges
                                , chord =
                                    Chord.chord
                                        PitchClass.c
                                        ChordClass.dominantSeventh
                                }
                                |> List.map Test.Util.voicingToString
                                |> List.isEmpty
                                |> not
                    in
                    resultIsNonEmpty
                        |> Expect.true "List of generated voicings was empty."
            , test "should not generate voicings of triads" <|
                \_ ->
                    let
                        results =
                            Classical.thirdInversion
                                { ranges =
                                    Classical.satbRanges
                                , chord =
                                    Chord.chord
                                        PitchClass.c
                                        ChordClass.major
                                }
                                |> List.map Test.Util.voicingToString
                    in
                    Expect.equal results []
            , test "all voicings should have the seventh in the fourth voice" <|
                \_ ->
                    let
                        results =
                            Classical.thirdInversion
                                { ranges =
                                    Classical.satbRanges
                                , chord =
                                    Chord.chord
                                        PitchClass.c
                                        ChordClass.dominantSeventh
                                }
                    in
                    results
                        |> Test.Util.expectAllInList
                            (checkVoice .voiceFour PitchClass.bFlat)
            , test "voicings should not double the seventh" <|
                \_ ->
                    let
                        results =
                            Classical.thirdInversion
                                { ranges =
                                    Classical.satbRanges
                                , chord =
                                    Chord.chord
                                        PitchClass.c
                                        ChordClass.dominantSeventh
                                }
                    in
                    results
                        |> Test.Util.expectAllInList
                            (checkDoesNotDoublePitchClass PitchClass.bFlat)
            , test "voicings should not double the third" <|
                \_ ->
                    let
                        results =
                            Classical.thirdInversion
                                { ranges =
                                    Classical.satbRanges
                                , chord =
                                    Chord.chord
                                        PitchClass.c
                                        ChordClass.dominantSeventh
                                }
                    in
                    results
                        |> Test.Util.expectAllInList
                            (checkDoesNotDoublePitchClass PitchClass.e)
            , test "voicings should not double the fifth" <|
                \_ ->
                    let
                        results =
                            Classical.thirdInversion
                                { ranges =
                                    Classical.satbRanges
                                , chord =
                                    Chord.chord
                                        PitchClass.c
                                        ChordClass.dominantSeventh
                                }
                    in
                    results
                        |> Test.Util.expectAllInList
                            (checkDoesNotDoublePitchClass PitchClass.g)
            ]
        , describe "orderByBestVoiceLeading"
            [ test "should prefer voicings with common tones" <|
                \_ ->
                    let
                        fromVoicing =
                            Voicing.fourPart
                                Pitch.c4
                                { voiceOne = Interval.perfectOctave
                                , voiceTwo = Interval.perfectFifth
                                , voiceThree = Interval.majorThird
                                , voiceFour = Interval.perfectUnison
                                }

                        voicingWithNoCommonTones =
                            Voicing.fourPart
                                Pitch.d4
                                { voiceOne = Interval.perfectOctave
                                , voiceTwo = Interval.perfectFifth
                                , voiceThree = Interval.majorThird
                                , voiceFour = Interval.perfectUnison
                                }

                        voicingWithOneCommonTone =
                            Voicing.fourPart
                                Pitch.c4
                                { voiceOne = Interval.perfectFifth |> Interval.addOctave
                                , voiceTwo = Interval.perfectOctave
                                , voiceThree = Interval.perfectFifth
                                , voiceFour = Interval.perfectUnison
                                }

                        voicingWithTwoCommonTones =
                            Voicing.fourPart
                                Pitch.c4
                                { voiceOne = Interval.majorTenth
                                , voiceTwo = Interval.perfectFifth
                                , voiceThree = Interval.majorThird
                                , voiceFour = Interval.perfectUnison
                                }

                        result =
                            [ voicingWithNoCommonTones
                            , voicingWithTwoCommonTones
                            , voicingWithOneCommonTone
                            ]
                                |> List.sortWith
                                    (Classical.orderByBestVoiceLeading fromVoicing)

                        expected =
                            [ voicingWithTwoCommonTones
                            , voicingWithOneCommonTone
                            , voicingWithNoCommonTones
                            ]
                    in
                    Expect.equal expected result
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
                            ++ Test.Util.voicingToString voicing
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
                            ++ Test.Util.voicingToString voicing
                            ++ " to contain "
                            ++ PitchClass.toString expectedPitchClass
                            ++ ".\n"
           )


checkDoesNotDoublePitchClass :
    PitchClass.PitchClass
    -> Voicing.FourPartVoicing
    -> Maybe String
checkDoesNotDoublePitchClass pitchClassNotToInclude voicing =
    voicing
        |> Voicing.toPitchesFourPart
        |> (\{ voiceOne, voiceTwo, voiceThree, voiceFour } ->
                let
                    instancesOfPitchClass =
                        [ Pitch.pitchClass voiceOne
                        , Pitch.pitchClass voiceTwo
                        , Pitch.pitchClass voiceThree
                        , Pitch.pitchClass voiceFour
                        ]
                            |> List.filter ((==) pitchClassNotToInclude)
                            |> List.length
                in
                if instancesOfPitchClass > 1 then
                    Just <|
                        "Expected voicing "
                            ++ Test.Util.voicingToString voicing
                            ++ " not to double "
                            ++ PitchClass.toString pitchClassNotToInclude
                            ++ ".\n"

                else
                    Nothing
           )
