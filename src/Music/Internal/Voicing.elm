module Music.Internal.Voicing exposing
    ( Voicing
    , chord
    , commonToneCount
    , commonTones
    , compareByCommonTones
    , compareByContraryMotion
    , compareByParallelOctave
    , compareByTotalSemitoneDistance
    , compareByVoiceSemitoneDistance
    , containsParallelFifths
    , containsParallelOctaves
    , containsPitch
    , containsPitchInVoice
    , range
    , root
    , semitoneCenter
    , semitoneCenterOrder
    , semitoneDistance
    , toString
    , totalSemitoneDistance
    , usesContraryMotion
    , violatesLowIntervalLimits
    , voicing
    , withInstrumentRanges
    )

import Music.Internal.Chord as Chord
import Music.Internal.Interval as Interval
import Music.Internal.Octave as Octave
import Music.Internal.Pitch as Pitch


type Voicing
    = Voicing Chord.Chord Octave.Octave


voicing : Chord.Chord -> Octave.Octave -> Voicing
voicing ch theOctave =
    Voicing ch theOctave


chord : Voicing -> Chord.Chord
chord (Voicing ch theOctave) =
    ch


root : Voicing -> Pitch.Pitch
root (Voicing ch theOctave) =
    Chord.root ch
        |> Pitch.fromPitchClass theOctave


range :
    { getTopVoice : Voicing -> Pitch.Pitch
    , getBottomVoice : Voicing -> Pitch.Pitch
    }
    -> Voicing
    -> Interval.Interval
range { getTopVoice, getBottomVoice } theVoicing =
    Pitch.intervalBetween
        (getBottomVoice theVoicing)
        (getTopVoice theVoicing)



-- Generating voicings


withInstrumentRanges :
    List (Voicing -> Pitch.Pitch)
    -> List (ranges -> Pitch.Range)
    -> ranges
    -> Voicing
    -> Bool
withInstrumentRanges allVoices allRanges ranges theVoicing =
    List.map2
        (\voice aRange ->
            Pitch.isWithin aRange voice
        )
        (List.map (\fn -> fn theVoicing) allVoices)
        (List.map (\fn -> fn ranges) allRanges)
        |> List.all identity



-- Comparisons & predicates


usesContraryMotion : (Voicing -> Pitch.Pitch) -> (Voicing -> Pitch.Pitch) -> Voicing -> Voicing -> Bool
usesContraryMotion getVoiceOne getVoiceTwo voicingA voicingB =
    Maybe.map2
        (/=)
        (movesUpward (getVoiceOne voicingA) (getVoiceOne voicingB))
        (movesUpward (getVoiceTwo voicingA) (getVoiceTwo voicingB))
        |> Maybe.withDefault False


compareByContraryMotion : (Voicing -> Pitch.Pitch) -> (Voicing -> Pitch.Pitch) -> Voicing -> (Voicing -> Voicing -> Order)
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


forEachVoice : List (Voicing -> Pitch.Pitch) -> Voicing -> Voicing -> (Pitch.Pitch -> Pitch.Pitch -> a) -> List a
forEachVoice allVoices voicingA voicingB fn =
    allVoices
        |> List.map
            (\getPitch ->
                ( getPitch voicingA, getPitch voicingB )
            )
        |> List.map (\( a, b ) -> fn a b)


commonToneCount : List (Voicing -> Pitch.Pitch) -> Voicing -> Voicing -> Int
commonToneCount allVoices voicingA voicingB =
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


commonTones : List (Voicing -> Pitch.Pitch) -> Voicing -> Voicing -> List Pitch.Pitch
commonTones allVoices voicingA voicingB =
    forEachVoice allVoices
        voicingA
        voicingB
        (\a b ->
            if a == b then
                Just a

            else
                Nothing
        )
        |> List.filterMap identity
        |> List.sortBy Pitch.semitones


compareByCommonTones :
    List (Voicing -> Pitch.Pitch)
    -> Voicing
    -> (Voicing -> Voicing -> Order)
compareByCommonTones allVoices from =
    \a b ->
        compare (commonToneCount allVoices from b) (commonToneCount allVoices from a)


semitoneDistance : Pitch.Pitch -> Pitch.Pitch -> Int
semitoneDistance a b =
    abs (Pitch.semitones a - Pitch.semitones b)


semitoneCenter : Pitch.Pitch -> Pitch.Pitch -> Int
semitoneCenter a b =
    (Pitch.semitones a + Pitch.semitones b) // 2


semitoneCenterOrder :
    Int
    -> (Voicing -> Pitch.Pitch)
    -> (Voicing -> Pitch.Pitch)
    -> (Voicing -> Voicing -> Order)
semitoneCenterOrder goal getLowestVoice getHighestVoice =
    \a b ->
        compare
            (abs (goal - semitoneCenter (getLowestVoice a) (getHighestVoice a)))
            (abs (goal - semitoneCenter (getLowestVoice b) (getHighestVoice b)))


totalSemitoneDistance :
    List
        (Voicing
         -> Pitch.Pitch
        )
    -> Voicing
    -> Voicing
    -> Int
totalSemitoneDistance allVoices voicingA voicingB =
    forEachVoice allVoices
        voicingA
        voicingB
        semitoneDistance
        |> List.sum


compareByTotalSemitoneDistance :
    List (Voicing -> Pitch.Pitch)
    -> Voicing
    -> (Voicing -> Voicing -> Order)
compareByTotalSemitoneDistance allVoices from =
    \a b ->
        compare
            (totalSemitoneDistance allVoices from a)
            (totalSemitoneDistance allVoices from b)


voiceSemitoneDistance : (voicing -> Pitch.Pitch) -> voicing -> voicing -> Int
voiceSemitoneDistance getVoice voicingA voicingB =
    semitoneDistance (getVoice voicingA) (getVoice voicingB)


compareByVoiceSemitoneDistance :
    (Voicing -> Pitch.Pitch)
    -> Voicing
    -> (Voicing -> Voicing -> Order)
compareByVoiceSemitoneDistance getter from =
    \a b ->
        compare
            (voiceSemitoneDistance getter from a)
            (voiceSemitoneDistance getter from b)


containsPitch :
    Pitch.Pitch
    -> List (Voicing -> Pitch.Pitch)
    -> Voicing
    -> Bool
containsPitch pitch allVoices theVoicing =
    List.map (\fn -> fn theVoicing) allVoices
        |> List.member pitch


containsPitchInVoice :
    Pitch.Pitch
    -> (Voicing -> Pitch.Pitch)
    -> Voicing
    -> Bool
containsPitchInVoice pitch getter theVoicing =
    getter theVoicing == pitch


containsFactor :
    Interval.Interval
    -> (Voicing -> List Interval.Interval)
    -> Voicing
    -> Bool
containsFactor factor allFactors theVoicing =
    theVoicing
        |> allFactors
        |> List.map Interval.toSimple
        |> List.member factor


containsParallelIntervals :
    Interval.Interval
    -> (Voicing -> Pitch.Pitch)
    -> List (Voicing -> Interval.Interval)
    -> Voicing
    -> Voicing
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
        |> List.isEmpty
        |> not


compareByParallelOctave :
    (Voicing -> Pitch.Pitch)
    -> List (Voicing -> Interval.Interval)
    -> Voicing
    -> (Voicing -> Voicing -> Order)
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
    (Voicing -> Pitch.Pitch)
    -> List (Voicing -> Interval.Interval)
    -> Voicing
    -> Voicing
    -> Bool
containsParallelFifths getRoot allFactors voicingA voicingB =
    containsParallelIntervals Interval.perfectFifth getRoot allFactors voicingA voicingB


containsParallelOctaves :
    (Voicing -> Pitch.Pitch)
    -> List (Voicing -> Interval.Interval)
    -> Voicing
    -> Voicing
    -> Bool
containsParallelOctaves getRoot allFactors voicingA voicingB =
    containsParallelIntervals Interval.perfectUnison getRoot allFactors voicingA voicingB



-- Miscellaneous conversions


toString : List (Voicing -> Pitch.Pitch) -> Voicing -> String
toString allVoices v =
    allVoices
        |> List.map (\fn -> fn v |> Pitch.toString)
        |> String.join " "



-- Low interval limits


type alias LowIntervalLimit =
    { interval : Interval.Interval
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


violatesLowIntervalLimits : Interval.Interval -> Pitch.Pitch -> Bool
violatesLowIntervalLimits intervalToTest pitchToTest =
    List.map
        (intervalViolatesLowIntervalLimit intervalToTest pitchToTest)
        lowIntervalLimits
        |> List.any identity
        |> not


intervalViolatesLowIntervalLimit : Interval.Interval -> Pitch.Pitch -> LowIntervalLimit -> Bool
intervalViolatesLowIntervalLimit intervalToTest pitchToTest { interval, lowestAllowedPitch } =
    Interval.isEqualTo intervalToTest interval
        && Pitch.isLessThan pitchToTest lowestAllowedPitch
