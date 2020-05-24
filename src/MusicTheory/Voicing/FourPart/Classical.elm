module MusicTheory.Voicing.FourPart.Classical exposing
    ( firstInversion
    , optimizeVoiceLeading
    , orderByBestVoiceLeading
    , rootPosition
    , satbRanges
    , secondInversion
    , thirdInversion
    )

import List.Extra
import MusicTheory.Chord as Chord
import MusicTheory.InstrumentRanges as InstrumentRanges
import MusicTheory.Interval as Interval
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
        |> FourPart.withFilter
            (FourPart.containsParallelOctaves fromVoicing >> not)


orderByBestVoiceLeading :
    Voicing.FourPartVoicing
    -> (Voicing.FourPartVoicing -> Voicing.FourPartVoicing -> Order)
orderByBestVoiceLeading from to =
    FourPart.compareByCommonTones from to


rootPosition : FourPart.TechniqueInput -> List Voicing.FourPartVoicing
rootPosition { ranges, chord } =
    let
        allValidRoots =
            Pitch.allForPitchClass (Chord.root chord)
    in
    case categorizeChordTones chord of
        Just chordTones ->
            Util.Permutations.permutations2
                allValidRoots
                (allRootPositionVoicingClasses chordTones)
                Voicing.fourPart
                |> List.filter (FourPartUtil.withinRanges ranges)

        Nothing ->
            []


firstInversion : FourPart.TechniqueInput -> List Voicing.FourPartVoicing
firstInversion { ranges, chord } =
    let
        allValidRoots =
            Chord.root chord
                |> Pitch.allForPitchClass
    in
    case categorizeChordTones chord of
        Just chordTones ->
            Util.Permutations.permutations2
                allValidRoots
                (allFirstInversionVoicingClasses chordTones)
                Voicing.fourPart
                |> List.filter (FourPartUtil.withinRanges ranges)

        Nothing ->
            []


secondInversion : FourPart.TechniqueInput -> List Voicing.FourPartVoicing
secondInversion { ranges, chord } =
    let
        allValidRoots =
            Chord.root chord
                |> Pitch.allForPitchClass
    in
    case categorizeChordTones chord of
        Just chordTones ->
            Util.Permutations.permutations2
                allValidRoots
                (allSecondInversionVoicingClasses chordTones)
                Voicing.fourPart
                |> List.filter (FourPartUtil.withinRanges ranges)

        Nothing ->
            []


thirdInversion : FourPart.TechniqueInput -> List Voicing.FourPartVoicing
thirdInversion { ranges, chord } =
    let
        allValidRoots =
            Chord.root chord
                |> Pitch.allForPitchClass
    in
    case categorizeChordTones chord of
        Just chordTones ->
            Util.Permutations.permutations2
                allValidRoots
                (allThirdInversionVoicingClasses chordTones)
                Voicing.fourPart
                |> List.filter (FourPartUtil.withinRanges ranges)

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
                    -- or double the fifth or third
                    [ [ tones.root
                      , tones.third
                      , seventh
                      ]
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
