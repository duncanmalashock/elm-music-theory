module MusicTheory.Voicing.FourPart exposing
    ( Config
    , Ranges
    , TechniqueInput
    , commonTones
    , compareByCommonTones
    , config
    , containsParallelFifths
    , containsParallelOctaves
    , execute
    )

import MusicTheory.Chord as Chord
import MusicTheory.Interval as Interval
import MusicTheory.Pitch as Pitch
import MusicTheory.Voicing as Voicing
import MusicTheory.VoicingClass as VoicingClass


type alias ConfigInput =
    { ranges : Ranges
    , techniques : List (TechniqueInput -> List Voicing.FourPartVoicing)
    }


type Config
    = Config
        { ranges : Ranges
        , techniques : List (TechniqueInput -> List Voicing.FourPartVoicing)
        , filter : List (Voicing.FourPartVoicing -> Bool)
        , sort : List (Voicing.FourPartVoicing -> Voicing.FourPartVoicing -> Order)
        }


type alias TechniqueInput =
    { ranges : Ranges
    , chord : Chord.Chord
    }


type alias Ranges =
    { first : Pitch.Range
    , second : Pitch.Range
    , third : Pitch.Range
    , fourth : Pitch.Range
    }


config : ConfigInput -> Config
config { ranges, techniques } =
    Config
        { ranges = ranges
        , techniques = techniques
        , filter = []
        , sort = []
        }


execute : Config -> Chord.Chord -> List Voicing.FourPartVoicing
execute (Config theConfig) theChord =
    theConfig.techniques
        |> List.concatMap
            (\technique ->
                technique
                    { ranges = theConfig.ranges
                    , chord = theChord
                    }
            )


commonTones : Voicing.FourPartVoicing -> Voicing.FourPartVoicing -> Int
commonTones voicingA voicingB =
    let
        pitchesA =
            Voicing.toPitchesFourPart voicingA

        pitchesB =
            Voicing.toPitchesFourPart voicingB

        areCommonTones =
            [ pitchesA.voiceOne == pitchesB.voiceOne
            , pitchesA.voiceTwo == pitchesB.voiceTwo
            , pitchesA.voiceThree == pitchesB.voiceThree
            , pitchesA.voiceFour == pitchesB.voiceFour
            ]
    in
    areCommonTones
        |> List.filter identity
        |> List.length


compareByCommonTones :
    Voicing.FourPartVoicing
    -> (Voicing.FourPartVoicing -> Voicing.FourPartVoicing -> Order)
compareByCommonTones from =
    \a b ->
        compare (commonTones from b) (commonTones from a)


containsParallelFifths : Voicing.FourPartVoicing -> Voicing.FourPartVoicing -> Bool
containsParallelFifths voicingA voicingB =
    containsParallelIntervals Interval.perfectFifth voicingA voicingB


containsParallelOctaves : Voicing.FourPartVoicing -> Voicing.FourPartVoicing -> Bool
containsParallelOctaves voicingA voicingB =
    containsParallelIntervals Interval.perfectUnison voicingA voicingB


containsParallelIntervals : Interval.Interval -> Voicing.FourPartVoicing -> Voicing.FourPartVoicing -> Bool
containsParallelIntervals interval voicingA voicingB =
    let
        intervalsA =
            VoicingClass.allIntervalsFourPart
                (Voicing.voicingClassFourPart voicingA)

        intervalsB =
            VoicingClass.allIntervalsFourPart
                (Voicing.voicingClassFourPart voicingB)

        areParallelInterval a b =
            (Interval.toSimple a == interval)
                && (Interval.toSimple b == interval)

        matchParallelIntervals =
            [ areParallelInterval intervalsA.voiceFourToVoiceOne intervalsB.voiceFourToVoiceOne
            , areParallelInterval intervalsA.voiceFourToVoiceTwo intervalsB.voiceFourToVoiceTwo
            , areParallelInterval intervalsA.voiceThreeToVoiceOne intervalsB.voiceThreeToVoiceOne
            , areParallelInterval intervalsA.voiceFourToVoiceThree intervalsB.voiceFourToVoiceThree
            , areParallelInterval intervalsA.voiceThreeToVoiceTwo intervalsB.voiceThreeToVoiceTwo
            , areParallelInterval intervalsA.voiceTwoToVoiceOne intervalsB.voiceTwoToVoiceOne
            ]
    in
    matchParallelIntervals
        |> List.filter identity
        |> (\list -> List.length list > 0)
