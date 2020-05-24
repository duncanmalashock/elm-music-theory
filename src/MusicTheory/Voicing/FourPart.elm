module MusicTheory.Voicing.FourPart exposing
    ( Config
    , Ranges
    , TechniqueInput
    , commonTones
    , config
    , execute
    )

import List.Extra
import MusicTheory.Chord as Chord
import MusicTheory.Pitch as Pitch
import MusicTheory.Voicing as Voicing


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
