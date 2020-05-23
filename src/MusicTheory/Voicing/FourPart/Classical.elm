module MusicTheory.Voicing.FourPart.Classical exposing (..)

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


allRootPositionVoicingClasses :
    CategorizedChordTones
    -> List VoicingClass.FourPartVoicingClass
allRootPositionVoicingClasses tones =
    let
        triadWithDoubledRoot =
            [ tones.root
            , tones.third
            , tones.fifth
            ]

        triadWithDoubledThird =
            [ tones.third
            , tones.third
            , tones.fifth
            ]

        triadWithDoubledFifth =
            [ tones.third
            , tones.fifth
            , tones.fifth
            ]

        seventhArrangements =
            Maybe.map
                (\seventh ->
                    let
                        seventhWithAllTones =
                            [ tones.third
                            , tones.fifth
                            , seventh
                            ]

                        seventhWithDoubledRoot =
                            [ tones.root
                            , tones.third
                            , seventh
                            ]

                        seventhWithDoubledThird =
                            [ tones.third
                            , tones.third
                            , seventh
                            ]
                    in
                    [ seventhWithAllTones
                    , seventhWithDoubledRoot
                    , seventhWithDoubledThird
                    ]
                )
                tones.seventh
                |> Maybe.withDefault []
    in
    [ triadWithDoubledRoot
    , triadWithDoubledThird
    , triadWithDoubledFifth
    ]
        ++ seventhArrangements
        |> List.concatMap List.Extra.permutations
        |> List.map (\l -> Interval.perfectUnison :: l)
        |> List.filterMap FourPartUtil.chordToneListToVoicingClass


satbRanges : FourPart.Ranges
satbRanges =
    { first = InstrumentRanges.sopranoVoice
    , second = InstrumentRanges.altoVoice
    , third = InstrumentRanges.tenorVoice
    , fourth = InstrumentRanges.bassVoice
    }
