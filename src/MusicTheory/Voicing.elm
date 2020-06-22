module MusicTheory.Voicing exposing
    ( commonTones
    , compareByCommonTones
    , compareByContraryMotion
    , compareByParallelOctave
    , compareByTotalSemitoneDistance
    , compareByVoiceSemitoneDistance
    , containsParallelFifths
    , containsParallelOctaves
    , containsPitch
    , semitoneDistance
    , totalSemitoneDistance
    , usesContraryMotion
    )

import MusicTheory.Interval as Interval
import MusicTheory.Pitch as Pitch


usesContraryMotion : (voicing -> Pitch.Pitch) -> (voicing -> Pitch.Pitch) -> voicing -> voicing -> Bool
usesContraryMotion getVoiceOne getVoiceTwo voicingA voicingB =
    Maybe.map2
        (/=)
        (movesUpward (getVoiceOne voicingA) (getVoiceOne voicingB))
        (movesUpward (getVoiceTwo voicingA) (getVoiceTwo voicingB))
        |> Maybe.withDefault False


compareByContraryMotion : (voicing -> Pitch.Pitch) -> (voicing -> Pitch.Pitch) -> voicing -> (voicing -> voicing -> Order)
compareByContraryMotion getVoiceOne getVoiceTwo from =
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
            (usesContraryMotion getVoiceOne getVoiceTwo from a |> boolToInt)
            (usesContraryMotion getVoiceOne getVoiceTwo from b |> boolToInt)


movesUpward : Pitch.Pitch -> Pitch.Pitch -> Maybe Bool
movesUpward a b =
    if (Pitch.semitones a - Pitch.semitones b) == 0 then
        Nothing

    else if (Pitch.semitones a - Pitch.semitones b) > 0 then
        Just False

    else
        Just True


forEachVoice : List (voicing -> Pitch.Pitch) -> voicing -> voicing -> (Pitch.Pitch -> Pitch.Pitch -> a) -> List a
forEachVoice allVoices voicingA voicingB fn =
    allVoices
        |> List.map
            (\getPitch ->
                ( getPitch voicingA, getPitch voicingB )
            )
        |> List.map (\( a, b ) -> fn a b)


commonTones : List (voicing -> Pitch.Pitch) -> voicing -> voicing -> Int
commonTones allVoices voicingA voicingB =
    forEachVoice allVoices
        voicingA
        voicingB
        (\a b ->
            if a == b then
                1

            else
                0
        )
        |> List.sum


compareByCommonTones :
    List (voicing -> Pitch.Pitch)
    -> voicing
    -> (voicing -> voicing -> Order)
compareByCommonTones allVoices from =
    \a b ->
        compare (commonTones allVoices from b) (commonTones allVoices from a)


semitoneDistance : Pitch.Pitch -> Pitch.Pitch -> Int
semitoneDistance a b =
    abs (Pitch.semitones a - Pitch.semitones b)


totalSemitoneDistance : List (voicing -> Pitch.Pitch) -> voicing -> voicing -> Int
totalSemitoneDistance allVoices voicingA voicingB =
    forEachVoice allVoices
        voicingA
        voicingB
        semitoneDistance
        |> List.sum


compareByTotalSemitoneDistance :
    List (voicing -> Pitch.Pitch)
    -> voicing
    -> (voicing -> voicing -> Order)
compareByTotalSemitoneDistance allVoices from =
    \a b ->
        compare
            (totalSemitoneDistance allVoices from a)
            (totalSemitoneDistance allVoices from b)


voiceSemitoneDistance : (voicing -> Pitch.Pitch) -> voicing -> voicing -> Int
voiceSemitoneDistance getVoice voicingA voicingB =
    semitoneDistance (getVoice voicingA) (getVoice voicingB)


compareByVoiceSemitoneDistance :
    (voicing -> Pitch.Pitch)
    -> voicing
    -> (voicing -> voicing -> Order)
compareByVoiceSemitoneDistance getter from =
    \a b ->
        compare
            (voiceSemitoneDistance getter from a)
            (voiceSemitoneDistance getter from b)


containsPitch :
    Pitch.Pitch
    -> List (voicing -> Pitch.Pitch)
    -> voicing
    -> Bool
containsPitch pitch allVoices theVoicing =
    List.map (\fn -> fn theVoicing) allVoices
        |> List.member pitch


containsPitchInVoice :
    Pitch.Pitch
    -> (voicing -> Pitch.Pitch)
    -> voicing
    -> Bool
containsPitchInVoice pitch getter theVoicing =
    getter theVoicing == pitch


containsFactor :
    Interval.Interval
    -> (voicing -> List Interval.Interval)
    -> voicing
    -> Bool
containsFactor factor allFactors theVoicing =
    theVoicing
        |> allFactors
        |> List.map Interval.toSimple
        |> List.member factor


containsParallelIntervals :
    Interval.Interval
    -> (voicing -> Pitch.Pitch)
    -> List (voicing -> Interval.Interval)
    -> voicing
    -> voicing
    -> Bool
containsParallelIntervals interval getRoot allFactors voicingA voicingB =
    let
        areParallelInterval a b =
            (Interval.toSimple a == interval)
                && (Interval.toSimple b == interval)
                && (getRoot voicingA /= getRoot voicingB)

        matchParallelIntervals =
            List.map
                (\getInterval -> areParallelInterval (getInterval voicingA) (getInterval voicingB))
                allFactors
    in
    matchParallelIntervals
        |> List.filter identity
        |> (\list -> List.length list > 0)


compareByParallelOctave :
    (voicing -> Pitch.Pitch)
    -> List (voicing -> Interval.Interval)
    -> voicing
    -> (voicing -> voicing -> Order)
compareByParallelOctave getRoot allFactors from =
    let
        boolToInt bool =
            case bool of
                False ->
                    1

                True ->
                    0
    in
    \a b ->
        compare
            (containsParallelOctaves getRoot allFactors from b |> boolToInt)
            (containsParallelOctaves getRoot allFactors from a |> boolToInt)


containsParallelFifths :
    (voicing -> Pitch.Pitch)
    -> List (voicing -> Interval.Interval)
    -> voicing
    -> voicing
    -> Bool
containsParallelFifths getRoot allFactors voicingA voicingB =
    containsParallelIntervals Interval.perfectFifth getRoot allFactors voicingA voicingB


containsParallelOctaves :
    (voicing -> Pitch.Pitch)
    -> List (voicing -> Interval.Interval)
    -> voicing
    -> voicing
    -> Bool
containsParallelOctaves getRoot allFactors voicingA voicingB =
    containsParallelIntervals Interval.perfectUnison getRoot allFactors voicingA voicingB



-- Low interval limits


type alias LowIntervalLimit =
    { intervalInSemitones : Interval.Interval
    , lowestAllowedPitch : Pitch.Pitch
    }


lowIntervalLimitForInterval : Interval.Interval -> Pitch.Pitch -> LowIntervalLimit
lowIntervalLimitForInterval theInterval thePitch =
    LowIntervalLimit theInterval thePitch


lowIntervalLimits : List LowIntervalLimit
lowIntervalLimits =
    [ lowIntervalLimitForInterval Interval.minorSecond Pitch.e3
    , lowIntervalLimitForInterval Interval.majorSecond Pitch.eFlat3
    , lowIntervalLimitForInterval Interval.minorThird Pitch.c3
    , lowIntervalLimitForInterval Interval.majorThird Pitch.bFlat2
    , lowIntervalLimitForInterval Interval.perfectFourth Pitch.bFlat2
    , lowIntervalLimitForInterval Interval.augmentedFourth Pitch.bFlat2
    , lowIntervalLimitForInterval Interval.perfectFifth Pitch.bFlat1
    , lowIntervalLimitForInterval Interval.minorSixth Pitch.g2
    , lowIntervalLimitForInterval Interval.majorSixth Pitch.f2
    , lowIntervalLimitForInterval Interval.minorSeventh Pitch.f2
    , lowIntervalLimitForInterval Interval.majorSeventh Pitch.f2
    , lowIntervalLimitForInterval Interval.minorNinth Pitch.e2
    , lowIntervalLimitForInterval Interval.majorNinth Pitch.eFlat2
    , lowIntervalLimitForInterval Interval.minorTenth Pitch.c2
    , lowIntervalLimitForInterval Interval.majorTenth Pitch.bFlat1
    ]
