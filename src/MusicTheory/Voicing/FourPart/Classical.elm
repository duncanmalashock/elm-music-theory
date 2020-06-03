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
import MusicTheory.Voicing.FourPart as FourPart
import MusicTheory.Voicing.Util as VoicingUtil
import Util.Permutations


optimizeVoiceLeading : FourPart.Voicing -> FourPart.Config -> FourPart.Config
optimizeVoiceLeading fromVoicing config =
    config
        |> FourPart.withSort
            (orderByBestVoiceLeading fromVoicing)
        |> FourPart.withFilter
            (FourPart.containsParallelFifths fromVoicing >> not)


resolvesTendencyTonesCorrectly :
    FourPart.Voicing
    -> FourPart.Voicing
    -> Bool
resolvesTendencyTonesCorrectly lastVoicing nextVoicing =
    let
        resolvesByFifth : Bool
        resolvesByFifth =
            (FourPart.root nextVoicing |> Pitch.pitchClass)
                == (FourPart.root lastVoicing
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
                        FourPart.chord chord
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
                        FourPart.chord chord
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
                        FourPart.chord chord
                            |> Chord.chordClass
                            |> ChordClass.toIntervals
                            |> (\i -> List.member factor i)
                    )
    in
    if isDominantChord lastVoicing && resolvesByFifth then
        let
            checkResolution getter allowResolutionToFifth =
                ( FourPart.voicingClass lastVoicing, FourPart.voicingClass nextVoicing )
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

        score : FourPart.Voicing -> FourPart.Voicing -> Float
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


rootPosition : FourPart.TechniqueInput -> List FourPart.Voicing
rootPosition { ranges, chord } =
    case categorizeChordTones chord of
        Just chordTones ->
            Util.Permutations.permutations2
                Octave.allValid
                (allRootPositionVoicingClasses chordTones)
                (FourPart.voicing chord)
                |> List.filter (FourPart.withinRanges ranges)
                |> List.Extra.uniqueBy FourPart.toString

        Nothing ->
            []


firstInversion : FourPart.TechniqueInput -> List FourPart.Voicing
firstInversion { ranges, chord } =
    case categorizeChordTones chord of
        Just chordTones ->
            Util.Permutations.permutations2
                Octave.allValid
                (allFirstInversionVoicingClasses chordTones)
                (FourPart.voicing chord)
                |> List.filter (FourPart.withinRanges ranges)
                |> List.Extra.uniqueBy FourPart.toString

        Nothing ->
            []


secondInversion : FourPart.TechniqueInput -> List FourPart.Voicing
secondInversion { ranges, chord } =
    case categorizeChordTones chord of
        Just chordTones ->
            Util.Permutations.permutations2
                Octave.allValid
                (allSecondInversionVoicingClasses chordTones)
                (FourPart.voicing chord)
                |> List.filter (FourPart.withinRanges ranges)
                |> List.Extra.uniqueBy FourPart.toString

        Nothing ->
            []


thirdInversion : FourPart.TechniqueInput -> List FourPart.Voicing
thirdInversion { ranges, chord } =
    case categorizeChordTones chord of
        Just chordTones ->
            Util.Permutations.permutations2
                Octave.allValid
                (allThirdInversionVoicingClasses chordTones)
                (FourPart.voicing chord)
                |> List.filter (FourPart.withinRanges ranges)
                |> List.Extra.uniqueBy FourPart.toString

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


satbRanges : FourPart.Ranges
satbRanges =
    { first = InstrumentRanges.sopranoVoice
    , second = InstrumentRanges.altoVoice
    , third = InstrumentRanges.tenorVoice
    , fourth = InstrumentRanges.bassVoice
    }
