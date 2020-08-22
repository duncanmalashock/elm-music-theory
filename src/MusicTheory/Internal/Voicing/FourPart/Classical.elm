module MusicTheory.Internal.Voicing.FourPart.Classical exposing
    ( firstInversion
    , optimizeVoiceLeading
    , orderByBestVoiceLeading
    , resolvesTendencyTonesCorrectly
    , rootPosition
    , secondInversion
    , thirdInversion
    )

import List.Extra
import MusicTheory.Internal.Chord as Chord
import MusicTheory.Internal.ChordClass as ChordClass exposing (ChordClass(..))
import MusicTheory.Internal.Interval as Interval exposing (IntervalNumber(..))
import MusicTheory.Internal.Octave as Octave
import MusicTheory.Internal.Pitch as Pitch
import MusicTheory.Internal.Voicing as Voicing
import MusicTheory.Internal.Voicing.FourPart as FourPart
import MusicTheory.Internal.Voicing.Util as VoicingUtil
import MusicTheory.Internal.VoicingClass as VoicingClass


optimizeVoiceLeading :
    FourPart.Voicing
    -> Voicing.Config FourPart.VoicingClass FourPart.Ranges
    -> Voicing.Config FourPart.VoicingClass FourPart.Ranges
optimizeVoiceLeading fromVoicing config =
    config
        |> Voicing.withSort
            (orderByBestVoiceLeading fromVoicing)
        |> Voicing.withFilter
            (Voicing.containsParallelFifths Voicing.root FourPart.allFactors fromVoicing >> not)
        |> Voicing.withFilter
            (Voicing.containsParallelOctaves Voicing.root FourPart.allFactors fromVoicing >> not)


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
                            |> Chord.chordClass
                            |> ChordClass.toIntervals
                            |> (\i -> List.member factor i)
                    )

        isMinorChord : FourPart.Voicing -> Bool
        isMinorChord chord =
            [ Interval.minorThird
            ]
                |> List.all
                    (\factor ->
                        Voicing.chord chord
                            |> Chord.chordClass
                            |> ChordClass.toIntervals
                            |> (\i -> List.member factor i)
                    )

        isMajorChord : FourPart.Voicing -> Bool
        isMajorChord chord =
            [ Interval.majorThird
            ]
                |> List.all
                    (\factor ->
                        Voicing.chord chord
                            |> Chord.chordClass
                            |> ChordClass.toIntervals
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


rootPosition : Voicing.TechniqueInput FourPart.Ranges -> List FourPart.Voicing
rootPosition { ranges, chord } =
    case categorizeChordTones chord of
        Just chordTones ->
            voicingClassesforAllOctaves allRootPositionVoicingClasses chord ranges chordTones

        Nothing ->
            []


firstInversion : Voicing.TechniqueInput FourPart.Ranges -> List FourPart.Voicing
firstInversion { ranges, chord } =
    case categorizeChordTones chord of
        Just chordTones ->
            voicingClassesforAllOctaves allFirstInversionVoicingClasses chord ranges chordTones

        Nothing ->
            []


secondInversion : Voicing.TechniqueInput FourPart.Ranges -> List FourPart.Voicing
secondInversion { ranges, chord } =
    case categorizeChordTones chord of
        Just chordTones ->
            voicingClassesforAllOctaves allSecondInversionVoicingClasses chord ranges chordTones

        Nothing ->
            []


thirdInversion : Voicing.TechniqueInput FourPart.Ranges -> List FourPart.Voicing
thirdInversion { ranges, chord } =
    case categorizeChordTones chord of
        Just chordTones ->
            voicingClassesforAllOctaves allThirdInversionVoicingClasses chord ranges chordTones

        Nothing ->
            []


voicingClassesforAllOctaves :
    (CategorizedChordTones
     -> List FourPart.VoicingClass
    )
    -> Chord.Chord
    -> FourPart.Ranges
    -> CategorizedChordTones
    -> List (Voicing.Voicing FourPart.VoicingClass)
voicingClassesforAllOctaves generate chord ranges chordTones =
    List.concatMap
        (\oct ->
            List.map
                (\class -> Voicing.voicing chord oct class)
                (generate chordTones)
        )
        Octave.allValid
        |> List.filter (Voicing.withInstrumentRanges FourPart.allVoices FourPart.allRanges ranges)
        |> List.Extra.uniqueBy (Voicing.toString FourPart.allVoices)


intervalLimits =
    { twoToOne =
        { max = Interval.perfectOctave
        , min = Interval.perfectUnison
        }
    , threeToTwo =
        { max = Interval.perfectOctave
        , min = Interval.perfectUnison
        }
    , fourToThree =
        { max = Interval.perfectOctave |> Interval.addOctave
        , min = Interval.perfectUnison
        }
    }


allThirdInversionVoicingClasses :
    CategorizedChordTones
    -> List FourPart.VoicingClass
allThirdInversionVoicingClasses tones =
    case tones.seventh of
        Just seventh ->
            -- If it's a seventh chord,
            [ -- use one chord tone per voice
              [ tones.root
              , tones.third
              , tones.fifth
              ]

            -- or double the root
            , [ tones.root
              , tones.root
              , tones.third
              ]
            ]
                |> List.concatMap (generateVoicingClasses seventh)

        Nothing ->
            -- If it's a triad, no possible voicings
            []


allSecondInversionVoicingClasses :
    CategorizedChordTones
    -> List FourPart.VoicingClass
allSecondInversionVoicingClasses tones =
    case tones.seventh of
        Just seventh ->
            -- If it's a seventh chord
            [ -- use one chord tone per voice
              [ tones.root
              , tones.third
              , seventh
              ]

            -- or double the fifth
            , [ tones.third
              , tones.fifth
              , seventh
              ]
            ]
                |> List.concatMap (generateVoicingClasses tones.fifth)

        Nothing ->
            -- If it's a triad,
            [ -- double the fifth
              [ tones.third
              , tones.fifth
              , tones.root
              ]
            , -- or double the third
              [ tones.root
              , tones.third
              , tones.third
              ]
            ]
                |> List.concatMap (generateVoicingClasses tones.fifth)


allFirstInversionVoicingClasses :
    CategorizedChordTones
    -> List FourPart.VoicingClass
allFirstInversionVoicingClasses tones =
    case tones.seventh of
        Just seventh ->
            -- If it's a seventh chord
            [ -- use one chord tone per voice
              [ tones.root
              , tones.fifth
              , seventh
              ]

            -- or double the root
            , [ tones.root
              , tones.root
              , seventh
              ]
            ]
                |> List.concatMap (generateVoicingClasses tones.third)

        Nothing ->
            -- If it's a triad,
            [ -- double the root
              [ tones.root
              , tones.root
              , tones.fifth
              ]
            , -- or double the fifth
              [ tones.root
              , tones.fifth
              , tones.fifth
              ]
            ]
                |> List.concatMap (generateVoicingClasses tones.third)


allRootPositionVoicingClasses :
    CategorizedChordTones
    -> List FourPart.VoicingClass
allRootPositionVoicingClasses tones =
    case tones.seventh of
        Just seventh ->
            -- If it's a seventh chord
            [ -- use one chord tone per voice
              [ tones.third
              , tones.fifth
              , seventh
              ]

            -- or double the root
            , [ tones.third
              , tones.root
              , seventh
              ]
            ]
                |> List.concatMap (generateVoicingClasses tones.root)

        Nothing ->
            -- If it's a triad,
            [ -- double the root
              [ tones.root
              , tones.third
              , tones.fifth
              ]
            , -- or double the fifth
              [ tones.third
              , tones.fifth
              , tones.fifth
              ]
            ]
                |> List.concatMap (generateVoicingClasses tones.root)


generateVoicingClasses : Interval.Interval -> List Interval.Interval -> List FourPart.VoicingClass
generateVoicingClasses bottomVoice firstThreeVoiceOptions =
    VoicingClass.builder FourPart.VoicingClass
        |> VoicingClass.withThreeFactorsFrom firstThreeVoiceOptions
            { mustBeUnique = False }
        |> VoicingClass.withFactor bottomVoice { mustBeUnique = False }
        |> VoicingClass.execute
            { placeFactors = FourPart.placeFactors intervalLimits }


categorizeChordTones : Chord.Chord -> Maybe CategorizedChordTones
categorizeChordTones chord =
    Maybe.map2
        (\third fifth ->
            { root = Interval.perfectUnison
            , third = third
            , fifth = fifth
            , seventh = VoicingUtil.getFactor VoicingUtil.sevenths chord
            }
        )
        (VoicingUtil.getFactor VoicingUtil.thirds chord)
        (VoicingUtil.getFactor VoicingUtil.fifths chord)


type alias CategorizedChordTones =
    { root : Interval.Interval
    , third : Interval.Interval
    , fifth : Interval.Interval
    , seventh : Maybe Interval.Interval
    }
