module MusicTheory.Voicing exposing
    ( Config
    , TechniqueInput
    , Voicing
    , chord
    , commonTones
    , compareByCommonTones
    , compareByContraryMotion
    , compareByParallelOctave
    , compareByTotalSemitoneDistance
    , compareByVoiceSemitoneDistance
    , config
    , containsParallelFifths
    , containsParallelOctaves
    , containsPitch
    , execute
    , root
    , semitoneDistance
    , toString
    , totalSemitoneDistance
    , usesContraryMotion
    , voicing
    , voicingClass
    , withFilter
    , withSort
    , withinRanges
    )

import List.Extra
import MusicTheory.Chord as Chord
import MusicTheory.Interval as Interval
import MusicTheory.Octave as Octave
import MusicTheory.Pitch as Pitch


type Voicing voicingClass
    = Voicing Chord.Chord Octave.Octave voicingClass


voicing : Chord.Chord -> Octave.Octave -> voicingClass -> Voicing voicingClass
voicing ch theOctave vc =
    Voicing ch theOctave vc


chord : Voicing voicingClass -> Chord.Chord
chord (Voicing ch theOctave vc) =
    ch


root : Voicing voicingClass -> Pitch.Pitch
root (Voicing ch theOctave vc) =
    Chord.root ch
        |> Pitch.fromPitchClass theOctave


voicingClass : Voicing voicingClass -> voicingClass
voicingClass (Voicing ch theOctave vc) =
    vc



-- Generating voicings


type alias ConfigInput voicingClass ranges =
    { ranges : ranges
    , techniques : List (TechniqueInput ranges -> List (Voicing voicingClass))
    }


type Config voicingClass ranges
    = Config
        { ranges : ranges
        , techniques : List (TechniqueInput ranges -> List (Voicing voicingClass))
        , filter : List (Voicing voicingClass -> Bool)
        , sort : Voicing voicingClass -> Voicing voicingClass -> Order
        }


type alias TechniqueInput ranges =
    { ranges : ranges
    , chord : Chord.Chord
    }


withinRanges : List (Voicing voicingClass -> Pitch.Pitch) -> List (ranges -> Pitch.Range) -> ranges -> Voicing voicingClass -> Bool
withinRanges allVoices allRanges ranges theVoicing =
    List.map2
        (\voice range ->
            Pitch.isWithin range voice
        )
        (List.map (\fn -> fn theVoicing) allVoices)
        (List.map (\fn -> fn ranges) allRanges)
        |> List.all identity


config : ConfigInput voicingClass ranges -> Config voicingClass ranges
config { ranges, techniques } =
    Config
        { ranges = ranges
        , techniques = techniques
        , filter = []
        , sort = \a b -> EQ
        }


withFilter : (Voicing voicingClass -> Bool) -> Config voicingClass ranges -> Config voicingClass ranges
withFilter filterFn (Config theConfig) =
    Config
        { theConfig
            | filter = theConfig.filter ++ [ filterFn ]
        }


withSort :
    (Voicing voicingClass
     -> Voicing voicingClass
     -> Order
    )
    -> Config voicingClass ranges
    -> Config voicingClass ranges
withSort sortFn (Config theConfig) =
    Config
        { theConfig
            | sort = sortFn
        }


execute : List (Voicing voicingClass -> Pitch.Pitch) -> Chord.Chord -> Config voicingClass ranges -> List (Voicing voicingClass)
execute allVoices theChord (Config theConfig) =
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
        |> List.Extra.uniqueBy (toString allVoices)
        |> List.sortWith theConfig.sort



-- Comparisons & predicates


usesContraryMotion : (Voicing voicingClass -> Pitch.Pitch) -> (Voicing voicingClass -> Pitch.Pitch) -> Voicing voicingClass -> Voicing voicingClass -> Bool
usesContraryMotion getVoiceOne getVoiceTwo voicingA voicingB =
    Maybe.map2
        (/=)
        (movesUpward (getVoiceOne voicingA) (getVoiceOne voicingB))
        (movesUpward (getVoiceTwo voicingA) (getVoiceTwo voicingB))
        |> Maybe.withDefault False


compareByContraryMotion : (Voicing voicingClass -> Pitch.Pitch) -> (Voicing voicingClass -> Pitch.Pitch) -> Voicing voicingClass -> (Voicing voicingClass -> Voicing voicingClass -> Order)
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


forEachVoice : List (Voicing voicingClass -> Pitch.Pitch) -> Voicing voicingClass -> Voicing voicingClass -> (Pitch.Pitch -> Pitch.Pitch -> a) -> List a
forEachVoice allVoices voicingA voicingB fn =
    allVoices
        |> List.map
            (\getPitch ->
                ( getPitch voicingA, getPitch voicingB )
            )
        |> List.map (\( a, b ) -> fn a b)


commonTones : List (Voicing voicingClass -> Pitch.Pitch) -> Voicing voicingClass -> Voicing voicingClass -> Int
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
    List (Voicing voicingClass -> Pitch.Pitch)
    -> Voicing voicingClass
    -> (Voicing voicingClass -> Voicing voicingClass -> Order)
compareByCommonTones allVoices from =
    \a b ->
        compare (commonTones allVoices from b) (commonTones allVoices from a)


semitoneDistance : Pitch.Pitch -> Pitch.Pitch -> Int
semitoneDistance a b =
    abs (Pitch.semitones a - Pitch.semitones b)


totalSemitoneDistance : List (Voicing voicingClass -> Pitch.Pitch) -> Voicing voicingClass -> Voicing voicingClass -> Int
totalSemitoneDistance allVoices voicingA voicingB =
    forEachVoice allVoices
        voicingA
        voicingB
        semitoneDistance
        |> List.sum


compareByTotalSemitoneDistance :
    List (Voicing voicingClass -> Pitch.Pitch)
    -> Voicing voicingClass
    -> (Voicing voicingClass -> Voicing voicingClass -> Order)
compareByTotalSemitoneDistance allVoices from =
    \a b ->
        compare
            (totalSemitoneDistance allVoices from a)
            (totalSemitoneDistance allVoices from b)


voiceSemitoneDistance : (voicing -> Pitch.Pitch) -> voicing -> voicing -> Int
voiceSemitoneDistance getVoice voicingA voicingB =
    semitoneDistance (getVoice voicingA) (getVoice voicingB)


compareByVoiceSemitoneDistance :
    (Voicing voicingClass -> Pitch.Pitch)
    -> Voicing voicingClass
    -> (Voicing voicingClass -> Voicing voicingClass -> Order)
compareByVoiceSemitoneDistance getter from =
    \a b ->
        compare
            (voiceSemitoneDistance getter from a)
            (voiceSemitoneDistance getter from b)


containsPitch :
    Pitch.Pitch
    -> List (Voicing voicingClass -> Pitch.Pitch)
    -> Voicing voicingClass
    -> Bool
containsPitch pitch allVoices theVoicing =
    List.map (\fn -> fn theVoicing) allVoices
        |> List.member pitch


containsPitchInVoice :
    Pitch.Pitch
    -> (Voicing voicingClass -> Pitch.Pitch)
    -> Voicing voicingClass
    -> Bool
containsPitchInVoice pitch getter theVoicing =
    getter theVoicing == pitch


containsFactor :
    Interval.Interval
    -> (Voicing voicingClass -> List Interval.Interval)
    -> Voicing voicingClass
    -> Bool
containsFactor factor allFactors theVoicing =
    theVoicing
        |> allFactors
        |> List.map Interval.toSimple
        |> List.member factor


containsParallelIntervals :
    Interval.Interval
    -> (Voicing voicingClass -> Pitch.Pitch)
    -> List (Voicing voicingClass -> Interval.Interval)
    -> Voicing voicingClass
    -> Voicing voicingClass
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
    (Voicing voicingClass -> Pitch.Pitch)
    -> List (Voicing voicingClass -> Interval.Interval)
    -> Voicing voicingClass
    -> (Voicing voicingClass -> Voicing voicingClass -> Order)
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
    (Voicing voicingClass -> Pitch.Pitch)
    -> List (Voicing voicingClass -> Interval.Interval)
    -> Voicing voicingClass
    -> Voicing voicingClass
    -> Bool
containsParallelFifths getRoot allFactors voicingA voicingB =
    containsParallelIntervals Interval.perfectFifth getRoot allFactors voicingA voicingB


containsParallelOctaves :
    (Voicing voicingClass -> Pitch.Pitch)
    -> List (Voicing voicingClass -> Interval.Interval)
    -> Voicing voicingClass
    -> Voicing voicingClass
    -> Bool
containsParallelOctaves getRoot allFactors voicingA voicingB =
    containsParallelIntervals Interval.perfectUnison getRoot allFactors voicingA voicingB



-- Miscellaneous conversions


toString : List (Voicing voicingClass -> Pitch.Pitch) -> Voicing voicingClass -> String
toString allVoices v =
    allVoices
        |> List.map (\fn -> fn v |> Pitch.toString)
        |> String.join " "



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
