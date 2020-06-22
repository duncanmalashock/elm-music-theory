module MusicTheory.Voicing.FourPart exposing
    ( Config
    , Pitches
    , Ranges
    , TechniqueInput
    , Voicing
    , VoicingClass
    , allFactors
    , allIntervals
    , allVoices
    , chord
    , chordToneListToVoicingClass
    , config
    , execute
    , getVoiceFour
    , getVoiceOne
    , getVoiceThree
    , getVoiceTwo
    , root
    , toPitches
    , toString
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
import Util.Basic


type Voicing
    = Voicing Chord.Chord Octave.Octave VoicingClass


voicing : Chord.Chord -> Octave.Octave -> VoicingClass -> Voicing
voicing ch octave vc =
    Voicing ch octave vc


type alias Pitches =
    { voiceOne : Pitch.Pitch
    , voiceTwo : Pitch.Pitch
    , voiceThree : Pitch.Pitch
    , voiceFour : Pitch.Pitch
    }


toPitches : Voicing -> Pitches
toPitches (Voicing ch octave vc) =
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


allVoices : List (Voicing -> Pitch.Pitch)
allVoices =
    [ toPitches >> .voiceOne
    , toPitches >> .voiceTwo
    , toPitches >> .voiceThree
    , toPitches >> .voiceFour
    ]


allFactors : List (Voicing -> Interval.Interval)
allFactors =
    [ voicingClass >> .voiceOne
    , voicingClass >> .voiceTwo
    , voicingClass >> .voiceThree
    , voicingClass >> .voiceFour
    ]


getVoiceOne : Voicing -> Pitch.Pitch
getVoiceOne =
    toPitches >> .voiceOne


getVoiceTwo : Voicing -> Pitch.Pitch
getVoiceTwo =
    toPitches >> .voiceTwo


getVoiceThree : Voicing -> Pitch.Pitch
getVoiceThree =
    toPitches >> .voiceThree


getVoiceFour : Voicing -> Pitch.Pitch
getVoiceFour =
    toPitches >> .voiceFour


type alias VoicingClass =
    { voiceOne : Interval.Interval
    , voiceTwo : Interval.Interval
    , voiceThree : Interval.Interval
    , voiceFour : Interval.Interval
    }


allIntervals : VoicingClass -> IntervalList
allIntervals vc =
    { fourToOne =
        Interval.subtract vc.voiceFour vc.voiceOne
    , fourToTwo =
        Interval.subtract vc.voiceFour vc.voiceTwo
    , threeToOne =
        Interval.subtract vc.voiceThree vc.voiceOne
    , fourToThree =
        Interval.subtract vc.voiceFour vc.voiceThree
    , threeToTwo =
        Interval.subtract vc.voiceThree vc.voiceTwo
    , twoToOne =
        Interval.subtract vc.voiceTwo vc.voiceOne
    }


type alias IntervalList =
    { fourToOne : Interval.Interval
    , fourToTwo : Interval.Interval
    , threeToOne : Interval.Interval
    , fourToThree : Interval.Interval
    , threeToTwo : Interval.Interval
    , twoToOne : Interval.Interval
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


toList : Pitches -> List Pitch.Pitch
toList { voiceOne, voiceTwo, voiceThree, voiceFour } =
    [ voiceOne, voiceTwo, voiceThree, voiceFour ]


voicingClass : Voicing -> VoicingClass
voicingClass (Voicing ch octave vc) =
    vc


chord : Voicing -> Chord.Chord
chord (Voicing ch octave vc) =
    ch


root : Voicing -> Pitch.Pitch
root (Voicing ch octave vc) =
    Chord.root ch
        |> Pitch.fromPitchClass octave



-- generating voicings


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
