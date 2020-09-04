module Music.Internal.Voicing.FourPart.Classical exposing
    ( firstInversion
    , orderByBestVoiceLeading
    , resolvesTendencyTonesCorrectly
    , rootPosition
    , secondInversion
    , thirdInversion
    )

import Music.Internal.Chord as Chord
import Music.Internal.ChordType as ChordType exposing (ChordType(..))
import Music.Internal.Interval as Interval exposing (IntervalNumber(..))
import Music.Internal.Pitch as Pitch
import Music.Internal.Voicing as Voicing
import Music.Internal.Voicing.FourPart as FourPart
import Music.Internal.Voicing.Util as VoicingUtil
import Music.Internal.VoicingClass as VoicingClass


rootPosition : FourPart.VoicingMethod
rootPosition =
    [ FourPart.custom
        categorizeChordTones
        (\tones ->
            FourPart.selectFactors
                |> FourPart.withThreeFactorsFrom
                    (case tones.seventh of
                        Just seventh ->
                            [ tones.third
                            , tones.fifth
                            , seventh
                            ]

                        Nothing ->
                            [ tones.root
                            , tones.third
                            , tones.fifth
                            ]
                    )
                |> FourPart.withFactor tones.root
                |> FourPart.placeSelectedFactors spacingLimits
        )
    , FourPart.custom
        categorizeChordTones
        (\tones ->
            FourPart.selectFactors
                |> FourPart.withThreeFactorsFrom
                    (case tones.seventh of
                        Just seventh ->
                            [ tones.third
                            , tones.root
                            , seventh
                            ]

                        Nothing ->
                            [ tones.third
                            , tones.fifth
                            , tones.fifth
                            ]
                    )
                |> FourPart.withFactor tones.root
                |> FourPart.placeSelectedFactors spacingLimits
        )
    ]
        |> FourPart.combineVoicingMethods


firstInversion : FourPart.VoicingMethod
firstInversion =
    [ FourPart.custom
        categorizeChordTones
        (\tones ->
            FourPart.selectFactors
                |> FourPart.withThreeFactorsFrom
                    (case tones.seventh of
                        Just seventh ->
                            [ tones.root
                            , tones.fifth
                            , seventh
                            ]

                        Nothing ->
                            [ tones.root
                            , tones.root
                            , tones.fifth
                            ]
                    )
                |> FourPart.withFactor tones.third
                |> FourPart.placeSelectedFactors spacingLimits
        )
    , FourPart.custom
        categorizeChordTones
        (\tones ->
            FourPart.selectFactors
                |> FourPart.withThreeFactorsFrom
                    (case tones.seventh of
                        Just seventh ->
                            [ tones.root
                            , tones.root
                            , seventh
                            ]

                        Nothing ->
                            [ tones.root
                            , tones.fifth
                            , tones.fifth
                            ]
                    )
                |> FourPart.withFactor tones.third
                |> FourPart.placeSelectedFactors spacingLimits
        )
    ]
        |> FourPart.combineVoicingMethods


secondInversion : FourPart.VoicingMethod
secondInversion =
    [ FourPart.custom
        categorizeChordTones
        (\tones ->
            FourPart.selectFactors
                |> FourPart.withThreeFactorsFrom
                    (case tones.seventh of
                        Just seventh ->
                            [ tones.root
                            , tones.third
                            , seventh
                            ]

                        Nothing ->
                            [ tones.third
                            , tones.fifth
                            , tones.root
                            ]
                    )
                |> FourPart.withFactor tones.fifth
                |> FourPart.placeSelectedFactors spacingLimits
        )
    , FourPart.custom
        categorizeChordTones
        (\tones ->
            FourPart.selectFactors
                |> FourPart.withThreeFactorsFrom
                    (case tones.seventh of
                        Just seventh ->
                            [ tones.third
                            , tones.fifth
                            , seventh
                            ]

                        Nothing ->
                            [ tones.root
                            , tones.third
                            , tones.third
                            ]
                    )
                |> FourPart.withFactor tones.fifth
                |> FourPart.placeSelectedFactors spacingLimits
        )
    ]
        |> FourPart.combineVoicingMethods


thirdInversion : FourPart.VoicingMethod
thirdInversion =
    [ FourPart.custom
        categorizeChordTones
        (\tones ->
            case tones.seventh of
                Just seventh ->
                    FourPart.selectFactors
                        |> FourPart.withThreeFactorsFrom
                            [ tones.root
                            , tones.third
                            , tones.fifth
                            ]
                        |> FourPart.withFactor seventh
                        |> FourPart.placeSelectedFactors spacingLimits

                Nothing ->
                    FourPart.noSelection
        )
    , FourPart.custom
        categorizeChordTones
        (\tones ->
            case tones.seventh of
                Just seventh ->
                    FourPart.selectFactors
                        |> FourPart.withThreeFactorsFrom
                            [ tones.root
                            , tones.root
                            , tones.third
                            ]
                        |> FourPart.withFactor seventh
                        |> FourPart.placeSelectedFactors spacingLimits

                Nothing ->
                    FourPart.noSelection
        )
    ]
        |> FourPart.combineVoicingMethods


spacingLimits : FourPart.SpacingLimits
spacingLimits =
    { twoToOne =
        Interval.range
            Interval.perfectUnison
            Interval.perfectOctave
    , threeToTwo =
        Interval.range
            Interval.perfectUnison
            Interval.perfectOctave
    , fourToThree =
        Interval.range
            Interval.perfectUnison
            (Interval.perfectOctave |> Interval.addOctave)
    }


categorizeChordTones : ChordType.ChordType -> Maybe CategorizedChordTones
categorizeChordTones chordType =
    Maybe.map2
        (\third fifth ->
            { root = Interval.perfectUnison
            , third = third
            , fifth = fifth
            , seventh = VoicingUtil.getFactor VoicingUtil.sevenths chordType
            }
        )
        (VoicingUtil.getFactor VoicingUtil.thirds chordType)
        (VoicingUtil.getFactor VoicingUtil.fifths chordType)


type alias CategorizedChordTones =
    { root : Interval.Interval
    , third : Interval.Interval
    , fifth : Interval.Interval
    , seventh : Maybe Interval.Interval
    }



-- Voice leading sort/filter


resolvesTendencyTonesCorrectly :
    FourPart.Voicing
    -> FourPart.Voicing
    -> Bool
resolvesTendencyTonesCorrectly lastVoicing nextVoicing =
    let
        resolvesByFifth : Bool
        resolvesByFifth =
            (Voicing.root nextVoicing |> Pitch.pitchClass)
                == (Voicing.root lastVoicing
                        |> Pitch.transposeDown Interval.perfectFifth
                        |> Pitch.pitchClass
                   )

        isDominantChord : FourPart.Voicing -> Bool
        isDominantChord chord =
            [ Interval.majorThird
            , Interval.minorSeventh
            ]
                |> List.all
                    (\factor ->
                        Voicing.chord chord
                            |> Chord.chordType
                            |> ChordType.toIntervals
                            |> (\i -> List.member factor i)
                    )

        isMinorChord : FourPart.Voicing -> Bool
        isMinorChord chord =
            [ Interval.minorThird
            ]
                |> List.all
                    (\factor ->
                        Voicing.chord chord
                            |> Chord.chordType
                            |> ChordType.toIntervals
                            |> (\i -> List.member factor i)
                    )

        isMajorChord : FourPart.Voicing -> Bool
        isMajorChord chord =
            [ Interval.majorThird
            ]
                |> List.all
                    (\factor ->
                        Voicing.chord chord
                            |> Chord.chordType
                            |> ChordType.toIntervals
                            |> (\i -> List.member factor i)
                    )
    in
    if isDominantChord lastVoicing && resolvesByFifth then
        let
            checkResolution getter allowResolutionToFifth =
                ( Voicing.voicingClass lastVoicing, Voicing.voicingClass nextVoicing )
                    |> (\( last, next ) ->
                            if Interval.toSimple (getter last) == Interval.majorThird then
                                (Interval.toSimple (getter next) == Interval.perfectUnison)
                                    || ((Interval.toSimple (getter next) == Interval.perfectFifth)
                                            && allowResolutionToFifth
                                       )

                            else if Interval.toSimple (getter last) == Interval.minorSeventh then
                                if isMinorChord nextVoicing then
                                    Interval.toSimple (getter next) == Interval.minorThird

                                else if isMajorChord nextVoicing then
                                    Interval.toSimple (getter next) == Interval.majorThird

                                else
                                    True

                            else
                                True
                       )
        in
        [ checkResolution .voiceOne False
        , checkResolution .voiceTwo True
        , checkResolution .voiceThree True
        , checkResolution .voiceFour True
        ]
            |> List.all identity

    else
        True


orderByBestVoiceLeading :
    FourPart.Voicing
    -> (FourPart.Voicing -> FourPart.Voicing -> Order)
orderByBestVoiceLeading from =
    let
        orderToNumber : Order -> Float
        orderToNumber ord =
            case ord of
                LT ->
                    -1

                EQ ->
                    0

                GT ->
                    1

        voiceOneSemitoneDistanceWeight =
            2

        voiceTwoSemitoneDistanceWeight =
            1

        voiceThreeSemitoneDistanceWeight =
            1

        voiceFourSemitoneDistanceWeight =
            0

        contraryMotionWeight =
            2

        totalSemitoneDistanceWeight =
            1

        commonToneWeight =
            1

        tendencyTonesWeight =
            10

        score : FourPart.Voicing -> FourPart.Voicing -> Float
        score a b =
            [ ( Voicing.compareByCommonTones FourPart.allVoices from a b, commonToneWeight )
            , ( Voicing.compareByTotalSemitoneDistance FourPart.allVoices from a b, totalSemitoneDistanceWeight )
            , ( Voicing.compareByContraryMotion FourPart.getVoiceFour FourPart.getVoiceThree from a b, contraryMotionWeight )
            , ( Voicing.compareByVoiceSemitoneDistance FourPart.getVoiceOne from a b, voiceOneSemitoneDistanceWeight )
            , ( Voicing.compareByVoiceSemitoneDistance FourPart.getVoiceTwo from a b, voiceTwoSemitoneDistanceWeight )
            , ( Voicing.compareByVoiceSemitoneDistance FourPart.getVoiceThree from a b, voiceThreeSemitoneDistanceWeight )
            , ( Voicing.compareByVoiceSemitoneDistance FourPart.getVoiceFour from a b, voiceFourSemitoneDistanceWeight )
            , ( compareByTendencyToneResolution from a b, tendencyTonesWeight )
            ]
                |> List.map
                    (\( comp, weight ) ->
                        orderToNumber comp * weight
                    )
                |> List.sum
    in
    \a b ->
        compare (score a b) 0


compareByTendencyToneResolution :
    FourPart.Voicing
    -> (FourPart.Voicing -> FourPart.Voicing -> Order)
compareByTendencyToneResolution from =
    let
        boolToInt bool =
            case bool of
                False ->
                    0

                True ->
                    1
    in
    \a b ->
        compare
            (resolvesTendencyTonesCorrectly from b |> boolToInt)
            (resolvesTendencyTonesCorrectly from a |> boolToInt)
