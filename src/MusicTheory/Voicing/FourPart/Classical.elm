module MusicTheory.Voicing.FourPart.Classical exposing
    ( firstInversion
    , optimizeVoiceLeading
    , orderByBestVoiceLeading
    , resolvesTendencyTonesCorrectly
    , rootPosition
    , secondInversion
    , thirdInversion
    )

import List.Extra
import MusicTheory.Chord as Chord
import MusicTheory.ChordClass as ChordClass exposing (ChordClass(..))
import MusicTheory.Interval as Interval exposing (IntervalNumber(..))
import MusicTheory.Octave as Octave
import MusicTheory.Pitch as Pitch
import MusicTheory.Voicing as Voicing
import MusicTheory.Voicing.FourPart as FourPart
import MusicTheory.Voicing.Util as VoicingUtil
import Util.Permutations


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

        parallelOctaveWeight =
            10

        tendencyTonesWeight =
            5

        score : FourPart.Voicing -> FourPart.Voicing -> Float
        score a b =
            [ ( Voicing.compareByCommonTones FourPart.allVoices from a b, commonToneWeight )
            , ( Voicing.compareByTotalSemitoneDistance FourPart.allVoices from a b, totalSemitoneDistanceWeight )
            , ( Voicing.compareByContraryMotion FourPart.getVoiceFour FourPart.getVoiceThree from a b, contraryMotionWeight )
            , ( Voicing.compareByVoiceSemitoneDistance FourPart.getVoiceOne from a b, voiceOneSemitoneDistanceWeight )
            , ( Voicing.compareByVoiceSemitoneDistance FourPart.getVoiceTwo from a b, voiceTwoSemitoneDistanceWeight )
            , ( Voicing.compareByVoiceSemitoneDistance FourPart.getVoiceThree from a b, voiceThreeSemitoneDistanceWeight )
            , ( Voicing.compareByVoiceSemitoneDistance FourPart.getVoiceFour from a b, voiceFourSemitoneDistanceWeight )
            , ( Voicing.compareByParallelOctave Voicing.root FourPart.allFactors from a b, parallelOctaveWeight )
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
            Util.Permutations.permutations2
                Octave.allValid
                (allRootPositionVoicingClasses chordTones)
                (Voicing.voicing chord)
                |> List.filter (Voicing.withinRanges FourPart.allVoices FourPart.allRanges ranges)
                |> List.Extra.uniqueBy (Voicing.toString FourPart.allVoices)

        Nothing ->
            []


firstInversion : Voicing.TechniqueInput FourPart.Ranges -> List FourPart.Voicing
firstInversion { ranges, chord } =
    case categorizeChordTones chord of
        Just chordTones ->
            Util.Permutations.permutations2
                Octave.allValid
                (allFirstInversionVoicingClasses chordTones)
                (Voicing.voicing chord)
                |> List.filter (Voicing.withinRanges FourPart.allVoices FourPart.allRanges ranges)
                |> List.Extra.uniqueBy (Voicing.toString FourPart.allVoices)

        Nothing ->
            []


secondInversion : Voicing.TechniqueInput FourPart.Ranges -> List FourPart.Voicing
secondInversion { ranges, chord } =
    case categorizeChordTones chord of
        Just chordTones ->
            Util.Permutations.permutations2
                Octave.allValid
                (allSecondInversionVoicingClasses chordTones)
                (Voicing.voicing chord)
                |> List.filter (Voicing.withinRanges FourPart.allVoices FourPart.allRanges ranges)
                |> List.Extra.uniqueBy (Voicing.toString FourPart.allVoices)

        Nothing ->
            []


thirdInversion : Voicing.TechniqueInput FourPart.Ranges -> List FourPart.Voicing
thirdInversion { ranges, chord } =
    case categorizeChordTones chord of
        Just chordTones ->
            Util.Permutations.permutations2
                Octave.allValid
                (allThirdInversionVoicingClasses chordTones)
                (Voicing.voicing chord)
                |> List.filter (Voicing.withinRanges FourPart.allVoices FourPart.allRanges ranges)
                |> List.Extra.uniqueBy (Voicing.toString FourPart.allVoices)

        Nothing ->
            []


allThirdInversionVoicingClasses :
    CategorizedChordTones
    -> List FourPart.VoicingClass
allThirdInversionVoicingClasses tones =
    case tones.seventh of
        Just seventh ->
            -- If it's a seventh chord, use one chord tone per voice
            -- or double the root
            [ [ tones.root
              , tones.third
              , tones.fifth
              ]
            , [ tones.root
              , tones.root
              , tones.third
              ]
            ]
                |> List.concatMap List.Extra.permutations
                |> List.map (\l -> seventh :: l)
                |> List.filterMap FourPart.chordToneListToVoicingClass

        Nothing ->
            -- If it's a triad, no possible voicings
            []


allSecondInversionVoicingClasses :
    CategorizedChordTones
    -> List FourPart.VoicingClass
allSecondInversionVoicingClasses tones =
    let
        validChordTonesAboveRoot =
            case tones.seventh of
                Just seventh ->
                    -- If it's a seventh chord, use one chord tone per voice
                    [ [ tones.root
                      , tones.third
                      , seventh
                      ]

                    -- or double the fifth
                    , [ tones.third
                      , tones.fifth
                      , seventh
                      ]
                    ]

                Nothing ->
                    -- If it's a triad, double the fifth or third
                    [ [ tones.root
                      , tones.third
                      , tones.fifth
                      ]
                    , [ tones.root
                      , tones.third
                      , tones.third
                      ]
                    ]
    in
    validChordTonesAboveRoot
        |> List.concatMap List.Extra.permutations
        |> List.map (\l -> tones.fifth :: l)
        |> List.filterMap FourPart.chordToneListToVoicingClass


allFirstInversionVoicingClasses :
    CategorizedChordTones
    -> List FourPart.VoicingClass
allFirstInversionVoicingClasses tones =
    let
        validChordTonesAboveRoot =
            case tones.seventh of
                Just seventh ->
                    -- If it's a seventh chord, use one chord tone per voice
                    -- or double the root
                    [ [ tones.root
                      , tones.fifth
                      , seventh
                      ]
                    , [ tones.root
                      , tones.root
                      , seventh
                      ]
                    ]

                Nothing ->
                    -- If it's a triad, double the root or fifth
                    [ [ tones.root
                      , tones.root
                      , tones.fifth
                      ]
                    , [ tones.root
                      , tones.fifth
                      , tones.fifth
                      ]
                    ]
    in
    validChordTonesAboveRoot
        |> List.concatMap List.Extra.permutations
        |> List.map (\l -> tones.third :: l)
        |> List.filterMap FourPart.chordToneListToVoicingClass


allRootPositionVoicingClasses :
    CategorizedChordTones
    -> List FourPart.VoicingClass
allRootPositionVoicingClasses tones =
    let
        validChordTonesAboveRoot =
            case tones.seventh of
                Just seventh ->
                    -- If it's a seventh chord, use one chord tone per voice
                    [ [ tones.third
                      , tones.fifth
                      , seventh
                      ]
                    , [ tones.root
                      , tones.third
                      , seventh
                      ]
                    , [ tones.third
                      , tones.third
                      , seventh
                      ]
                    ]

                Nothing ->
                    -- If it's a triad, double the root
                    [ [ tones.root
                      , tones.third
                      , tones.fifth
                      ]
                    , [ tones.third
                      , tones.fifth
                      , tones.fifth
                      ]
                    ]
    in
    validChordTonesAboveRoot
        |> List.concatMap List.Extra.permutations
        |> List.map (\l -> Interval.perfectUnison :: l)
        |> List.filterMap FourPart.chordToneListToVoicingClass


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
