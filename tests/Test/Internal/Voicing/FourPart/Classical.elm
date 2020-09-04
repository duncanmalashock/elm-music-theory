module Test.Internal.Voicing.FourPart.Classical exposing (..)

import Expect
import Music.Chord as Chord
import Music.Internal.Chord as Chord
import Music.Internal.ChordType as ChordType
import Music.Internal.InstrumentRanges as InstrumentRanges
import Music.Internal.Interval as Interval
import Music.Internal.Octave as Octave
import Music.Internal.Pitch as Pitch
import Music.Internal.PitchClass as PitchClass
import Music.Internal.Voicing as Voicing
import Music.Internal.Voicing.FourPart as FourPart
import Music.Internal.Voicing.FourPart.Classical as Classical
import Test exposing (..)
import Test.Internal.Util


satbRanges : FourPart.Ranges
satbRanges =
    { voiceOne = InstrumentRanges.sopranoVoice
    , voiceTwo = InstrumentRanges.altoVoice
    , voiceThree = InstrumentRanges.tenorVoice
    , voiceFour = InstrumentRanges.bassVoice
    }


all : Test
all =
    describe "all"
        [ describe "rootPosition"
            [ test "should generate voicings of the chord" <|
                \_ ->
                    let
                        resultIsNonEmpty =
                            Chord.voiceFourParts
                                satbRanges
                                [ Classical.rootPosition ]
                                (Chord.chord
                                    PitchClass.c
                                    ChordType.dominantSeventh
                                )
                                |> List.map Test.Internal.Util.voicingToString
                                |> List.isEmpty
                                |> not
                    in
                    resultIsNonEmpty
                        |> Expect.true "List of generated voicings was empty."
            , test "all voicings should have root in the fourth voice" <|
                \_ ->
                    let
                        results =
                            Chord.voiceFourParts
                                satbRanges
                                [ Classical.rootPosition ]
                                (Chord.chord
                                    PitchClass.c
                                    ChordType.dominantSeventh
                                )
                    in
                    results
                        |> Test.Internal.Util.expectAllInList
                            (checkVoice .voiceFour PitchClass.c)
            , test "all voicings of a triad should include the third" <|
                \_ ->
                    let
                        results =
                            Chord.voiceFourParts
                                satbRanges
                                [ Classical.rootPosition ]
                                (Chord.chord
                                    PitchClass.c
                                    ChordType.major
                                )
                    in
                    results
                        |> Test.Internal.Util.expectAllInList
                            (checkIncludesPitchClass PitchClass.e)
            , test "all voicings of a seventh chord should include the seventh" <|
                \_ ->
                    let
                        results =
                            Chord.voiceFourParts
                                satbRanges
                                [ Classical.rootPosition ]
                                (Chord.chord
                                    PitchClass.c
                                    ChordType.dominantSeventh
                                )
                    in
                    results
                        |> Test.Internal.Util.expectAllInList
                            (checkIncludesPitchClass PitchClass.bFlat)
            ]
        , describe "firstInversion"
            [ test "should generate voicings of the chord" <|
                \_ ->
                    let
                        resultIsNonEmpty =
                            Chord.voiceFourParts
                                satbRanges
                                [ Classical.firstInversion ]
                                (Chord.chord
                                    PitchClass.c
                                    ChordType.dominantSeventh
                                )
                                |> List.map Test.Internal.Util.voicingToString
                                |> List.isEmpty
                                |> not
                    in
                    resultIsNonEmpty
                        |> Expect.true "List of generated voicings was empty."
            , test "all voicings should have the third in the fourth voice" <|
                \_ ->
                    let
                        results =
                            Chord.voiceFourParts
                                satbRanges
                                [ Classical.firstInversion ]
                                (Chord.chord
                                    PitchClass.c
                                    ChordType.dominantSeventh
                                )
                    in
                    results
                        |> Test.Internal.Util.expectAllInList
                            (checkVoice .voiceFour PitchClass.e)
            , test "voicings of triads should not double the third" <|
                \_ ->
                    let
                        results =
                            Chord.voiceFourParts
                                satbRanges
                                [ Classical.firstInversion ]
                                (Chord.chord
                                    PitchClass.c
                                    ChordType.major
                                )
                    in
                    results
                        |> Test.Internal.Util.expectAllInList
                            (checkDoesNotDoublePitchClass PitchClass.e)
            , test "voicings of seventh chords should not double the third" <|
                \_ ->
                    let
                        results =
                            Chord.voiceFourParts
                                satbRanges
                                [ Classical.firstInversion ]
                                (Chord.chord
                                    PitchClass.c
                                    ChordType.dominantSeventh
                                )
                    in
                    results
                        |> Test.Internal.Util.expectAllInList
                            (checkDoesNotDoublePitchClass PitchClass.e)
            , test "voicings of seventh chords should not double the fifth" <|
                \_ ->
                    let
                        results =
                            Chord.voiceFourParts
                                satbRanges
                                [ Classical.firstInversion ]
                                (Chord.chord
                                    PitchClass.c
                                    ChordType.dominantSeventh
                                )
                    in
                    results
                        |> Test.Internal.Util.expectAllInList
                            (checkDoesNotDoublePitchClass PitchClass.g)
            ]
        , describe "secondInversion"
            [ test "should generate voicings of the chord" <|
                \_ ->
                    let
                        resultIsNonEmpty =
                            Chord.voiceFourParts
                                satbRanges
                                [ Classical.secondInversion ]
                                (Chord.chord
                                    PitchClass.c
                                    ChordType.dominantSeventh
                                )
                                |> List.map Test.Internal.Util.voicingToString
                                |> List.isEmpty
                                |> not
                    in
                    resultIsNonEmpty
                        |> Expect.true "List of generated voicings was empty."
            , test "all voicings should have the fifth in the fourth voice" <|
                \_ ->
                    let
                        results =
                            Chord.voiceFourParts
                                satbRanges
                                [ Classical.secondInversion ]
                                (Chord.chord
                                    PitchClass.c
                                    ChordType.dominantSeventh
                                )
                    in
                    results
                        |> Test.Internal.Util.expectAllInList
                            (checkVoice .voiceFour PitchClass.g)
            , test "voicings of triads should not double the root" <|
                \_ ->
                    let
                        results =
                            Chord.voiceFourParts
                                satbRanges
                                [ Classical.secondInversion ]
                                (Chord.chord
                                    PitchClass.c
                                    ChordType.major
                                )
                    in
                    results
                        |> Test.Internal.Util.expectAllInList
                            (checkDoesNotDoublePitchClass PitchClass.c)
            , test "voicings of seventh chords should not double the seventh" <|
                \_ ->
                    let
                        results =
                            Chord.voiceFourParts
                                satbRanges
                                [ Classical.secondInversion ]
                                (Chord.chord
                                    PitchClass.c
                                    ChordType.dominantSeventh
                                )
                    in
                    results
                        |> Test.Internal.Util.expectAllInList
                            (checkDoesNotDoublePitchClass PitchClass.bFlat)
            ]
        , describe "thirdInversion"
            [ test "should generate voicings of the chord" <|
                \_ ->
                    let
                        resultIsNonEmpty =
                            Chord.voiceFourParts
                                satbRanges
                                [ Classical.thirdInversion ]
                                (Chord.chord
                                    PitchClass.c
                                    ChordType.dominantSeventh
                                )
                                |> List.map Test.Internal.Util.voicingToString
                                |> List.isEmpty
                                |> not
                    in
                    resultIsNonEmpty
                        |> Expect.true "List of generated voicings was empty."
            , test "should not generate voicings of triads" <|
                \_ ->
                    let
                        results =
                            Chord.voiceFourParts
                                satbRanges
                                [ Classical.thirdInversion ]
                                (Chord.chord
                                    PitchClass.c
                                    ChordType.major
                                )
                                |> List.map Test.Internal.Util.voicingToString
                    in
                    Expect.equal results []
            , test "all voicings should have the seventh in the fourth voice" <|
                \_ ->
                    let
                        results =
                            Chord.voiceFourParts
                                satbRanges
                                [ Classical.thirdInversion ]
                                (Chord.chord
                                    PitchClass.c
                                    ChordType.dominantSeventh
                                )
                    in
                    results
                        |> Test.Internal.Util.expectAllInList
                            (checkVoice .voiceFour PitchClass.bFlat)
            , test "voicings should not double the seventh" <|
                \_ ->
                    let
                        results =
                            Chord.voiceFourParts
                                satbRanges
                                [ Classical.thirdInversion ]
                                (Chord.chord
                                    PitchClass.c
                                    ChordType.dominantSeventh
                                )
                    in
                    results
                        |> Test.Internal.Util.expectAllInList
                            (checkDoesNotDoublePitchClass PitchClass.bFlat)
            , test "voicings should not double the third" <|
                \_ ->
                    let
                        results =
                            Chord.voiceFourParts
                                satbRanges
                                [ Classical.thirdInversion ]
                                (Chord.chord
                                    PitchClass.c
                                    ChordType.dominantSeventh
                                )
                    in
                    results
                        |> Test.Internal.Util.expectAllInList
                            (checkDoesNotDoublePitchClass PitchClass.e)
            , test "voicings should not double the fifth" <|
                \_ ->
                    let
                        results =
                            Chord.voiceFourParts
                                satbRanges
                                [ Classical.thirdInversion ]
                                (Chord.chord
                                    PitchClass.c
                                    ChordType.dominantSeventh
                                )
                    in
                    results
                        |> Test.Internal.Util.expectAllInList
                            (checkDoesNotDoublePitchClass PitchClass.g)
            ]
        , describe "resolvesTendencyTonesCorrectly"
            [ test "in D7 to G major, F# should resolve to G, and C should resolve to B" <|
                \_ ->
                    let
                        d7voicing =
                            Voicing.voicing
                                (Chord.chord PitchClass.d ChordType.dominantSeventh)
                                Octave.three
                                { voiceOne = Interval.majorTenth |> Interval.addOctave
                                , voiceTwo = Interval.perfectTwelfth
                                , voiceThree = Interval.minorSeventh
                                , voiceFour = Interval.perfectUnison
                                }

                        gVoicing =
                            Voicing.voicing
                                (Chord.chord PitchClass.g ChordType.major)
                                Octave.three
                                { voiceOne = Interval.perfectOctave |> Interval.addOctave
                                , voiceTwo = Interval.perfectTwelfth
                                , voiceThree = Interval.majorThird
                                , voiceFour = Interval.perfectUnison
                                }

                        result =
                            Classical.resolvesTendencyTonesCorrectly d7voicing gVoicing

                        expected =
                            True
                    in
                    Expect.equal expected result
            , test "in D7 to G minor, F# should resolve to G, and C should resolve to Bb" <|
                \_ ->
                    let
                        d7voicing =
                            Voicing.voicing
                                (Chord.chord PitchClass.d ChordType.dominantSeventh)
                                Octave.three
                                { voiceOne = Interval.majorTenth |> Interval.addOctave
                                , voiceTwo = Interval.perfectTwelfth
                                , voiceThree = Interval.minorSeventh
                                , voiceFour = Interval.perfectUnison
                                }

                        gMinorVoicing =
                            Voicing.voicing
                                (Chord.chord PitchClass.g ChordType.minor)
                                Octave.three
                                { voiceOne = Interval.perfectOctave |> Interval.addOctave
                                , voiceTwo = Interval.perfectTwelfth
                                , voiceThree = Interval.minorThird
                                , voiceFour = Interval.perfectUnison
                                }

                        result =
                            Classical.resolvesTendencyTonesCorrectly d7voicing gMinorVoicing

                        expected =
                            True
                    in
                    Expect.equal expected result
            , test "in D7 to G, F# should not resolve to D" <|
                \_ ->
                    let
                        d7voicing =
                            Voicing.voicing
                                (Chord.chord PitchClass.d ChordType.dominantSeventh)
                                Octave.three
                                { voiceOne = Interval.majorTenth |> Interval.addOctave
                                , voiceTwo = Interval.perfectTwelfth
                                , voiceThree = Interval.minorSeventh
                                , voiceFour = Interval.perfectUnison
                                }

                        gMinorVoicing =
                            Voicing.voicing
                                (Chord.chord PitchClass.g ChordType.minor)
                                Octave.three
                                { voiceOne = Interval.perfectTwelfth
                                , voiceTwo = Interval.perfectOctave
                                , voiceThree = Interval.minorThird
                                , voiceFour = Interval.perfectUnison
                                }

                        result =
                            Classical.resolvesTendencyTonesCorrectly d7voicing gMinorVoicing

                        expected =
                            False
                    in
                    Expect.equal expected result
            ]
        ]


checkVoice :
    (FourPart.Pitches -> Pitch.Pitch)
    -> PitchClass.PitchClass
    -> FourPart.Voicing
    -> Maybe String
checkVoice getter expectedPitchClass voicing =
    voicing
        |> FourPart.toPitches
        |> getter
        |> (\pitch ->
                if Pitch.pitchClass pitch == expectedPitchClass then
                    Nothing

                else
                    Just <|
                        "Expected pitch class in voicing "
                            ++ Test.Internal.Util.voicingToString voicing
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
    -> FourPart.Voicing
    -> Maybe String
checkIncludesPitchClass expectedPitchClass voicing =
    voicing
        |> FourPart.toPitches
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
                            ++ Test.Internal.Util.voicingToString voicing
                            ++ " to contain "
                            ++ PitchClass.toString expectedPitchClass
                            ++ ".\n"
           )


checkDoesNotDoublePitchClass :
    PitchClass.PitchClass
    -> FourPart.Voicing
    -> Maybe String
checkDoesNotDoublePitchClass pitchClassNotToInclude voicing =
    voicing
        |> FourPart.toPitches
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
                            ++ Test.Internal.Util.voicingToString voicing
                            ++ " not to double "
                            ++ PitchClass.toString pitchClassNotToInclude
                            ++ ".\n"

                else
                    Nothing
           )
