module MusicTheory.Voicing.FourPart.Classical exposing
    ( firstInversion
    , optimizeVoiceLeading
    , orderByBestVoiceLeading
    , resolvesTendencyTonesCorrectly
    , rootPosition
    , satbRanges
    , secondInversion
    , thirdInversion
    )

import List.Extra
import MusicTheory.Chord as Chord
import MusicTheory.ChordClass as ChordClass exposing (ChordClass(..))
import MusicTheory.InstrumentRanges as InstrumentRanges
import MusicTheory.Interval as Interval exposing (IntervalNumber(..))
import MusicTheory.Octave as Octave
import MusicTheory.Pitch as Pitch
import MusicTheory.Voicing as Voicing
import MusicTheory.Voicing.FourPart as FourPart
import MusicTheory.Voicing.FourPart.Util as FourPartUtil
import MusicTheory.VoicingClass as VoicingClass
import Util.Permutations


optimizeVoiceLeading : Voicing.FourPartVoicing -> FourPart.Config -> FourPart.Config
optimizeVoiceLeading fromVoicing config =
    config
        |> FourPart.withSort
            (orderByBestVoiceLeading fromVoicing)
        |> FourPart.withFilter
            (FourPart.containsParallelFifths fromVoicing >> not)


resolvesTendencyTonesCorrectly :
    Voicing.FourPartVoicing
    -> Voicing.FourPartVoicing
    -> Bool
resolvesTendencyTonesCorrectly lastVoicing nextVoicing =
    let
        resolvesByFifth : Bool
        resolvesByFifth =
            (Voicing.rootFourPart nextVoicing |> Pitch.pitchClass)
                == (Voicing.rootFourPart lastVoicing
                        |> Pitch.transposeDown Interval.perfectFifth
                        |> Pitch.pitchClass
                   )

        isDominantChord : Voicing.FourPartVoicing -> Bool
        isDominantChord chord =
            [ Interval.majorThird
            , Interval.minorSeventh
            ]
                |> List.all
                    (\factor ->
                        Voicing.chordFourPart chord
                            |> Chord.chordClass
                            |> ChordClass.toIntervals
                            |> (\i -> List.member factor i)
                    )

        isMinorChord : Voicing.FourPartVoicing -> Bool
        isMinorChord chord =
            [ Interval.minorThird
            ]
                |> List.all
                    (\factor ->
                        Voicing.chordFourPart chord
                            |> Chord.chordClass
                            |> ChordClass.toIntervals
                            |> (\i -> List.member factor i)
                    )

        isMajorChord : Voicing.FourPartVoicing -> Bool
        isMajorChord chord =
            [ Interval.majorThird
            ]
                |> List.all
                    (\factor ->
                        Voicing.chordFourPart chord
                            |> Chord.chordClass
                            |> ChordClass.toIntervals
                            |> (\i -> List.member factor i)
                    )
    in
    if isDominantChord lastVoicing && resolvesByFifth then
        let
            checkResolution getter allowResolutionToFifth =
                ( Voicing.voicingClassFourPart lastVoicing, Voicing.voicingClassFourPart nextVoicing )
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
    Voicing.FourPartVoicing
    -> (Voicing.FourPartVoicing -> Voicing.FourPartVoicing -> Order)
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
            1

        voiceTwoSemitoneDistanceWeight =
            1

        voiceThreeSemitoneDistanceWeight =
            2

        voiceFourSemitoneDistanceWeight =
            2

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

        score : Voicing.FourPartVoicing -> Voicing.FourPartVoicing -> Float
        score a b =
            [ ( FourPart.compareByCommonTones from a b, commonToneWeight )
            , ( FourPart.compareBySemitoneDistance from a b, totalSemitoneDistanceWeight )
            , ( FourPart.compareByContraryMotion from a b, contraryMotionWeight )
            , ( FourPart.compareByVoiceSemitoneDistance .voiceOne from a b, voiceOneSemitoneDistanceWeight )
            , ( FourPart.compareByVoiceSemitoneDistance .voiceTwo from a b, voiceTwoSemitoneDistanceWeight )
            , ( FourPart.compareByVoiceSemitoneDistance .voiceThree from a b, voiceThreeSemitoneDistanceWeight )
            , ( FourPart.compareByVoiceSemitoneDistance .voiceFour from a b, voiceFourSemitoneDistanceWeight )
            , ( FourPart.compareByParallelOctave from a b, parallelOctaveWeight )
            , ( compareByTendencyToneResolution from a b, tendencyTonesWeight )
            ]
                |> List.map
                    (\( comp, weight ) ->
                        orderToNumber comp * weight
                    )
                |> List.sum
                |> Debug.log "score"
    in
    \a b ->
        compare (score a b) 0


compareByTendencyToneResolution :
    Voicing.FourPartVoicing
    -> (Voicing.FourPartVoicing -> Voicing.FourPartVoicing -> Order)
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


rootPosition : FourPart.TechniqueInput -> List Voicing.FourPartVoicing
rootPosition { ranges, chord } =
    case categorizeChordTones chord of
        Just chordTones ->
            Util.Permutations.permutations2
                Octave.allValid
                (allRootPositionVoicingClasses chordTones)
                (Voicing.fourPart chord)
                |> List.filter (FourPartUtil.withinRanges ranges)
                |> List.Extra.uniqueBy Voicing.fourPartToComparable

        Nothing ->
            []


firstInversion : FourPart.TechniqueInput -> List Voicing.FourPartVoicing
firstInversion { ranges, chord } =
    case categorizeChordTones chord of
        Just chordTones ->
            Util.Permutations.permutations2
                Octave.allValid
                (allFirstInversionVoicingClasses chordTones)
                (Voicing.fourPart chord)
                |> List.filter (FourPartUtil.withinRanges ranges)
                |> List.Extra.uniqueBy Voicing.fourPartToComparable

        Nothing ->
            []


secondInversion : FourPart.TechniqueInput -> List Voicing.FourPartVoicing
secondInversion { ranges, chord } =
    case categorizeChordTones chord of
        Just chordTones ->
            Util.Permutations.permutations2
                Octave.allValid
                (allSecondInversionVoicingClasses chordTones)
                (Voicing.fourPart chord)
                |> List.filter (FourPartUtil.withinRanges ranges)
                |> List.Extra.uniqueBy Voicing.fourPartToComparable

        Nothing ->
            []


thirdInversion : FourPart.TechniqueInput -> List Voicing.FourPartVoicing
thirdInversion { ranges, chord } =
    case categorizeChordTones chord of
        Just chordTones ->
            Util.Permutations.permutations2
                Octave.allValid
                (allThirdInversionVoicingClasses chordTones)
                (Voicing.fourPart chord)
                |> List.filter (FourPartUtil.withinRanges ranges)
                |> List.Extra.uniqueBy Voicing.fourPartToComparable

        Nothing ->
            []


allThirdInversionVoicingClasses :
    CategorizedChordTones
    -> List VoicingClass.FourPartVoicingClass
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
                |> List.filterMap FourPartUtil.chordToneListToVoicingClass

        Nothing ->
            -- If it's a triad, no possible voicings
            []


allSecondInversionVoicingClasses :
    CategorizedChordTones
    -> List VoicingClass.FourPartVoicingClass
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
        |> List.filterMap FourPartUtil.chordToneListToVoicingClass


allFirstInversionVoicingClasses :
    CategorizedChordTones
    -> List VoicingClass.FourPartVoicingClass
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
        |> List.filterMap FourPartUtil.chordToneListToVoicingClass


allRootPositionVoicingClasses :
    CategorizedChordTones
    -> List VoicingClass.FourPartVoicingClass
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
        |> List.filterMap FourPartUtil.chordToneListToVoicingClass


categorizeChordTones : Chord.Chord -> Maybe CategorizedChordTones
categorizeChordTones chord =
    Maybe.map2
        (\third fifth ->
            { root = Interval.perfectUnison
            , third = third
            , fifth = fifth
            , seventh = FourPartUtil.getFactor FourPartUtil.sevenths chord
            }
        )
        (FourPartUtil.getFactor FourPartUtil.thirds chord)
        (FourPartUtil.getFactor FourPartUtil.fifths chord)


type alias CategorizedChordTones =
    { root : Interval.Interval
    , third : Interval.Interval
    , fifth : Interval.Interval
    , seventh : Maybe Interval.Interval
    }


satbRanges : FourPart.Ranges
satbRanges =
    { first = InstrumentRanges.sopranoVoice
    , second = InstrumentRanges.altoVoice
    , third = InstrumentRanges.tenorVoice
    , fourth = InstrumentRanges.bassVoice
    }
