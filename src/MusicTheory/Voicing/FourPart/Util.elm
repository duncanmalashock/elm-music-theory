module MusicTheory.Voicing.FourPart.Util exposing (..)

import MusicTheory.Chord as Chord
import MusicTheory.ChordClass as ChordClass
import MusicTheory.Interval as Interval
import MusicTheory.Pitch as Pitch
import MusicTheory.Voicing as Voicing
import MusicTheory.Voicing.FourPart as FourPart
import MusicTheory.VoicingClass as VoicingClass
import Util.Basic


thirds : List Interval.Interval
thirds =
    [ Interval.majorThird
    , Interval.minorThird
    ]


fifths : List Interval.Interval
fifths =
    [ Interval.diminishedFifth
    , Interval.perfectFifth
    , Interval.augmentedFifth
    ]


sevenths : List Interval.Interval
sevenths =
    [ Interval.diminishedSeventh
    , Interval.minorSeventh
    , Interval.majorSeventh
    ]


withinRanges : FourPart.Ranges -> Voicing.FourPartVoicing -> Bool
withinRanges { first, second, third, fourth } voicing =
    Voicing.toPitchesFourPart voicing
        |> (\voiced ->
                Pitch.isWithin first voiced.voiceOne
                    && Pitch.isWithin second voiced.voiceTwo
                    && Pitch.isWithin third voiced.voiceFour
                    && Pitch.isWithin fourth voiced.voiceFour
           )


getFactor : List Interval.Interval -> Chord.Chord -> Maybe Interval.Interval
getFactor factorMembers chord =
    ChordClass.toIntervals (Chord.chordClass chord)
        |> List.filter (\i -> List.member i factorMembers)
        |> List.head


chordToneListToVoicingClass : List Interval.Interval -> Maybe VoicingClass.FourPartVoicingClass
chordToneListToVoicingClass intervals =
    case intervals of
        i1 :: i2 :: i3 :: i4 :: _ ->
            { voiceOne = i4
            , voiceTwo = i3
            , voiceThree = i2
            , voiceFour = i1
            }
                |> correctIntervalOctaves
                |> Just

        _ ->
            Nothing


correctIntervalOctaves : VoicingClass.FourPartVoicingClass -> VoicingClass.FourPartVoicingClass
correctIntervalOctaves { voiceOne, voiceTwo, voiceThree, voiceFour } =
    let
        -- TODO: allow for larger (e.g. 10th) and smaller distances (unison) between voices
        voiceThreeCorrected =
            Util.Basic.while
                (\v3 -> Interval.semitones voiceFour >= Interval.semitones v3)
                Interval.addOctave
                voiceThree

        voiceTwoCorrected =
            Util.Basic.while
                (\v2 -> Interval.semitones voiceThreeCorrected >= Interval.semitones v2)
                Interval.addOctave
                voiceTwo

        voiceOneCorrected =
            Util.Basic.while
                (\v4 -> Interval.semitones voiceTwoCorrected >= Interval.semitones v4)
                Interval.addOctave
                voiceOne
    in
    { voiceOne = voiceOneCorrected
    , voiceTwo = voiceTwoCorrected
    , voiceThree = voiceThreeCorrected
    , voiceFour = voiceFour
    }
