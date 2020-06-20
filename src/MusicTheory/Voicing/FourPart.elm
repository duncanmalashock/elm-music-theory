module MusicTheory.Voicing.FourPart exposing
    ( Config
    , Pitches
    , Ranges
    , TechniqueInput
    , Voicing
    , VoicingClass
    , allIntervals
    , chord
    , chordToneListToVoicingClass
    , commonTones
    , compareByCommonTones
    , compareByContraryMotion
    , compareByParallelOctave
    , compareBySemitoneDistance
    , compareByVoiceSemitoneDistance
    , config
    , containsFactor
    , containsParallelFifths
    , containsParallelOctaves
    , containsPitch
    , containsPitchInVoice
    , execute
    , root
    , toPitches
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
import MusicTheory.Voicing.Part as Part
import Util.Basic


type Voicing
    = FourPartVoicing Chord.Chord Octave.Octave VoicingClass


voicing : Chord.Chord -> Octave.Octave -> VoicingClass -> Voicing
voicing ch octave vc =
    FourPartVoicing ch octave vc


type alias Pitches =
    { voiceOne : Pitch.Pitch
    , voiceTwo : Pitch.Pitch
    , voiceThree : Pitch.Pitch
    , voiceFour : Pitch.Pitch
    }


getVoices : List (Voicing -> Pitch.Pitch)
getVoices =
    [ toPitches >> .voiceOne
    , toPitches >> .voiceTwo
    , toPitches >> .voiceThree
    , toPitches >> .voiceFour
    ]


forEachPitch : Voicing -> Voicing -> (Pitch.Pitch -> Pitch.Pitch -> a) -> List a
forEachPitch voicingA voicingB fn =
    getVoices
        |> List.map
            (\getPitch ->
                ( getPitch voicingA, getPitch voicingB )
            )
        |> List.map (\( a, b ) -> fn a b)


forPitch : Voicing -> Voicing -> (Pitches -> Pitch.Pitch) -> (Pitch.Pitch -> Pitch.Pitch -> a) -> a
forPitch voicingA voicingB getPitch fn =
    fn (getPitch (toPitches voicingA)) (getPitch (toPitches voicingB))


type alias VoicingClass =
    { voiceOne : Interval.Interval
    , voiceTwo : Interval.Interval
    , voiceThree : Interval.Interval
    , voiceFour : Interval.Interval
    }


allIntervals : VoicingClass -> IntervalList
allIntervals vc =
    { voiceFourToVoiceOne =
        Interval.subtract vc.voiceFour vc.voiceOne
    , voiceFourToVoiceTwo =
        Interval.subtract vc.voiceFour vc.voiceTwo
    , voiceThreeToVoiceOne =
        Interval.subtract vc.voiceThree vc.voiceOne
    , voiceFourToVoiceThree =
        Interval.subtract vc.voiceFour vc.voiceThree
    , voiceThreeToVoiceTwo =
        Interval.subtract vc.voiceThree vc.voiceTwo
    , voiceTwoToVoiceOne =
        Interval.subtract vc.voiceTwo vc.voiceOne
    }


type alias IntervalList =
    { voiceFourToVoiceOne : Interval.Interval
    , voiceFourToVoiceTwo : Interval.Interval
    , voiceThreeToVoiceOne : Interval.Interval
    , voiceFourToVoiceThree : Interval.Interval
    , voiceThreeToVoiceTwo : Interval.Interval
    , voiceTwoToVoiceOne : Interval.Interval
    }


toString : Voicing -> String
toString v =
    v
        |> toPitches
        |> (\{ voiceOne, voiceTwo, voiceThree, voiceFour } ->
                String.join " "
                    [ Pitch.toString voiceFour
                    , Pitch.toString voiceThree
                    , Pitch.toString voiceTwo
                    , Pitch.toString voiceOne
                    ]
           )


toPitches : Voicing -> Pitches
toPitches (FourPartVoicing ch octave vc) =
    let
        theRoot =
            Chord.root ch
                |> Pitch.fromPitchClass octave
    in
    { voiceOne = Pitch.transposeUp vc.voiceOne theRoot
    , voiceTwo = Pitch.transposeUp vc.voiceTwo theRoot
    , voiceThree = Pitch.transposeUp vc.voiceThree theRoot
    , voiceFour = Pitch.transposeUp vc.voiceFour theRoot
    }


toList : Pitches -> List Pitch.Pitch
toList { voiceOne, voiceTwo, voiceThree, voiceFour } =
    [ voiceOne, voiceTwo, voiceThree, voiceFour ]


voicingClass : Voicing -> VoicingClass
voicingClass (FourPartVoicing ch octave vc) =
    vc


chord : Voicing -> Chord.Chord
chord (FourPartVoicing ch octave vc) =
    ch


root : Voicing -> Pitch.Pitch
root (FourPartVoicing ch octave vc) =
    Chord.root ch
        |> Pitch.fromPitchClass octave


type alias ConfigInput =
    { ranges : Ranges
    , techniques : List (TechniqueInput -> List Voicing)
    }


type Config
    = Config
        { ranges : Ranges
        , techniques : List (TechniqueInput -> List Voicing)
        , filter : List (Voicing -> Bool)
        , sort : Voicing -> Voicing -> Order
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


withinRanges : Ranges -> Voicing -> Bool
withinRanges { first, second, third, fourth } theVoicing =
    toPitches theVoicing
        |> (\voiced ->
                Pitch.isWithin first voiced.voiceOne
                    && Pitch.isWithin second voiced.voiceTwo
                    && Pitch.isWithin third voiced.voiceFour
                    && Pitch.isWithin fourth voiced.voiceFour
           )


config : ConfigInput -> Config
config { ranges, techniques } =
    Config
        { ranges = ranges
        , techniques = techniques
        , filter = []
        , sort = \a b -> EQ
        }


withFilter : (Voicing -> Bool) -> Config -> Config
withFilter filterFn (Config theConfig) =
    Config
        { theConfig
            | filter = theConfig.filter ++ [ filterFn ]
        }


withSort :
    (Voicing
     -> Voicing
     -> Order
    )
    -> Config
    -> Config
withSort sortFn (Config theConfig) =
    Config
        { theConfig
            | sort = sortFn
        }


execute : Chord.Chord -> Config -> List Voicing
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
        |> List.Extra.uniqueBy toString
        |> List.sortWith theConfig.sort


commonTones : Voicing -> Voicing -> Int
commonTones voicingA voicingB =
    forEachPitch voicingA voicingB Part.commonTones
        |> List.sum


compareByCommonTones :
    Voicing
    -> (Voicing -> Voicing -> Order)
compareByCommonTones from =
    \a b ->
        compare (commonTones from b) (commonTones from a)


totalSemitoneDistance : Voicing -> Voicing -> Int
totalSemitoneDistance voicingA voicingB =
    forEachPitch voicingA voicingB Part.semitoneDistance
        |> List.sum


voiceSemitoneDistance : (Pitches -> Pitch.Pitch) -> Voicing -> Voicing -> Int
voiceSemitoneDistance getter voicingA voicingB =
    forPitch voicingA voicingB getter Part.semitoneDistance


compareByVoiceSemitoneDistance :
    (Pitches -> Pitch.Pitch)
    -> Voicing
    -> (Voicing -> Voicing -> Order)
compareByVoiceSemitoneDistance getter from =
    \a b ->
        compare
            (voiceSemitoneDistance getter from a)
            (voiceSemitoneDistance getter from b)


compareByParallelOctave :
    Voicing
    -> (Voicing -> Voicing -> Order)
compareByParallelOctave from =
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
            (containsParallelOctaves from b |> boolToInt)
            (containsParallelOctaves from a |> boolToInt)


compareBySemitoneDistance :
    Voicing
    -> (Voicing -> Voicing -> Order)
compareBySemitoneDistance from =
    \a b ->
        compare (totalSemitoneDistance from a) (totalSemitoneDistance from b)


compareParts : Voicing -> Voicing -> (Pitches -> Pitch.Pitch) -> (Pitches -> Pitch.Pitch) -> Part.CompareParts
compareParts voicingA voicingB getPartA getPartB =
    Part.compareParts
        voicingA
        voicingB
        (toPitches >> getPartA)
        (toPitches >> getPartB)


usesContraryMotion : Voicing -> Voicing -> Bool
usesContraryMotion voicingA voicingB =
    compareParts voicingA voicingB .voiceThree .voiceFour
        |> Part.usesContraryMotion


compareByContraryMotion :
    Voicing
    -> (Voicing -> Voicing -> Order)
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
    Voicing
    -> Voicing
    -> Bool
containsParallelFifths voicingA voicingB =
    containsParallelIntervals Interval.perfectFifth voicingA voicingB


containsParallelOctaves :
    Voicing
    -> Voicing
    -> Bool
containsParallelOctaves voicingA voicingB =
    containsParallelIntervals Interval.perfectUnison voicingA voicingB


containsParallelIntervals :
    Interval.Interval
    -> Voicing
    -> Voicing
    -> Bool
containsParallelIntervals interval voicingA voicingB =
    let
        intervalsA =
            allIntervals
                (voicingClass voicingA)

        intervalsB =
            allIntervals
                (voicingClass voicingB)

        areParallelInterval a b =
            (Interval.toSimple a == interval)
                && (Interval.toSimple b == interval)
                && (root voicingA /= root voicingB)

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


containsPitch :
    Pitch.Pitch
    -> Voicing
    -> Bool
containsPitch pitch theVoicing =
    theVoicing
        |> toPitches
        |> toList
        |> List.member pitch


containsPitchInVoice :
    Pitch.Pitch
    -> (Pitches -> Pitch.Pitch)
    -> Voicing
    -> Bool
containsPitchInVoice pitch getter theVoicing =
    theVoicing
        |> toPitches
        |> (\pitches -> getter pitches == pitch)


containsFactor :
    Interval.Interval
    -> Voicing
    -> Bool
containsFactor factor theVoicing =
    theVoicing
        |> voicingClass
        |> (\{ voiceOne, voiceTwo, voiceThree, voiceFour } ->
                [ voiceOne, voiceTwo, voiceThree, voiceFour ]
           )
        |> List.map Interval.toSimple
        |> List.member factor


chordToneListToVoicingClass : List Interval.Interval -> Maybe VoicingClass
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


correctIntervalOctaves : VoicingClass -> VoicingClass
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
