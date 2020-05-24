module MusicTheory.Voicing.FourPart exposing
    ( Config
    , Ranges
    , TechniqueInput
    , commonTones
    , compareByCommonTones
    , compareByContraryMotion
    , compareBySemitoneDistance
    , config
    , containsParallelFifths
    , containsParallelOctaves
    , execute
    , totalSemitoneDistance
    , usesContraryMotion
    , withFilter
    , withSort
    )

import List.Extra
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
        , sort : Voicing.FourPartVoicing -> Voicing.FourPartVoicing -> Order
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
        , sort = \a b -> EQ
        }


withFilter : (Voicing.FourPartVoicing -> Bool) -> Config -> Config
withFilter filterFn (Config theConfig) =
    Config
        { theConfig
            | filter = theConfig.filter ++ [ filterFn ]
        }


withSort :
    (Voicing.FourPartVoicing
     -> Voicing.FourPartVoicing
     -> Order
    )
    -> Config
    -> Config
withSort sortFn (Config theConfig) =
    Config
        { theConfig
            | sort = sortFn
        }


execute : Chord.Chord -> Config -> List Voicing.FourPartVoicing
execute theChord (Config theConfig) =
    theConfig.techniques
        |> List.concatMap
            (\technique ->
                technique
                    { ranges = theConfig.ranges
                    , chord = theChord
                    }
            )
        |> (\candidates ->
                List.foldl List.filter candidates theConfig.filter
           )
        |> List.Extra.uniqueBy Voicing.fourPartToComparable
        |> List.sortWith theConfig.sort


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


totalSemitoneDistance : Voicing.FourPartVoicing -> Voicing.FourPartVoicing -> Int
totalSemitoneDistance voicingA voicingB =
    let
        pitchesA =
            Voicing.toPitchesFourPart voicingA

        pitchesB =
            Voicing.toPitchesFourPart voicingB

        semitoneDistance a b =
            abs (Pitch.semitones a - Pitch.semitones b)

        semitoneDistances =
            [ semitoneDistance pitchesA.voiceOne pitchesB.voiceOne
            , semitoneDistance pitchesA.voiceTwo pitchesB.voiceTwo
            , semitoneDistance pitchesA.voiceThree pitchesB.voiceThree
            , semitoneDistance pitchesA.voiceFour pitchesB.voiceFour
            ]
    in
    semitoneDistances
        |> List.sum


compareBySemitoneDistance :
    Voicing.FourPartVoicing
    -> (Voicing.FourPartVoicing -> Voicing.FourPartVoicing -> Order)
compareBySemitoneDistance from =
    \a b ->
        compare (totalSemitoneDistance from a) (totalSemitoneDistance from b)


usesContraryMotion : Voicing.FourPartVoicing -> Voicing.FourPartVoicing -> Bool
usesContraryMotion voicingA voicingB =
    let
        pitchesA =
            Voicing.toPitchesFourPart voicingA

        pitchesB =
            Voicing.toPitchesFourPart voicingB

        positiveSemitoneDistance : Pitch.Pitch -> Pitch.Pitch -> Maybe Bool
        positiveSemitoneDistance a b =
            if (Pitch.semitones a - Pitch.semitones b) == 0 then
                Nothing

            else if (Pitch.semitones a - Pitch.semitones b) > 0 then
                Just False

            else
                Just True
    in
    Maybe.map2
        (/=)
        (positiveSemitoneDistance pitchesA.voiceFour pitchesB.voiceFour)
        (positiveSemitoneDistance pitchesA.voiceThree pitchesB.voiceThree)
        |> Maybe.withDefault False


compareByContraryMotion :
    Voicing.FourPartVoicing
    -> (Voicing.FourPartVoicing -> Voicing.FourPartVoicing -> Order)
compareByContraryMotion from =
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
            (usesContraryMotion from b |> boolToInt)
            (usesContraryMotion from a |> boolToInt)


containsParallelFifths :
    Voicing.FourPartVoicing
    -> Voicing.FourPartVoicing
    -> Bool
containsParallelFifths voicingA voicingB =
    containsParallelIntervals Interval.perfectFifth voicingA voicingB


containsParallelOctaves :
    Voicing.FourPartVoicing
    -> Voicing.FourPartVoicing
    -> Bool
containsParallelOctaves voicingA voicingB =
    containsParallelIntervals Interval.perfectUnison voicingA voicingB


containsParallelIntervals :
    Interval.Interval
    -> Voicing.FourPartVoicing
    -> Voicing.FourPartVoicing
    -> Bool
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
