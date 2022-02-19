module Music.Internal.Voicing exposing
    ( Voicing
    , compareVoiceAtIndex
    , containsPitch
    , containsPitchAtVoice
    , init
    , initFromPitches
    , toPitches
    , toString
    )

import List.Extra
import Music.Internal.Interval as Interval
import Music.Internal.Pitch as Pitch


init :
    List
        { pitch : Pitch.Pitch
        , interval : Interval.Interval
        , sourceInterval : Interval.Interval
        }
    -> Voicing
init list =
    Voicing
        { voices =
            List.map
                (\input ->
                    Voice
                        { pitch = input.pitch
                        , interval = input.interval
                        , sourceInterval = input.sourceInterval
                        }
                )
                list
        }


initFromPitches :
    List Pitch.Pitch
    -> Voicing
initFromPitches list =
    Voicing
        { voices =
            List.map
                (\input ->
                    Voice
                        { pitch = input
                        , interval = Interval.perfectUnison
                        , sourceInterval = Interval.perfectUnison
                        }
                )
                list
        }


compareVoiceAtIndex : Int -> Voicing -> Voicing -> Maybe VoiceCompare
compareVoiceAtIndex index (Voicing voicingA) (Voicing voicingB) =
    let
        getAt : List Voice -> Maybe VoiceDetails
        getAt voices =
            List.Extra.getAt index voices
                |> Maybe.map
                    (\(Voice voiceDetails) -> voiceDetails)
    in
    case ( getAt voicingA.voices, getAt voicingB.voices ) of
        ( Just voiceA, Just voiceB ) ->
            Just
                { voiceA = voiceA
                , voiceB = voiceB
                }

        _ ->
            Nothing


containsPitch : Pitch.Pitch -> Voicing -> Bool
containsPitch pitch voicing =
    List.member pitch (toPitches voicing)


toPitches : Voicing -> List Pitch.Pitch
toPitches (Voicing details) =
    details.voices
        |> List.map (\(Voice voice) -> voice.pitch)


containsPitchAtVoice : Int -> Pitch.Pitch -> Voicing -> Bool
containsPitchAtVoice index pitch voicing =
    getVoiceAt index voicing
        |> Maybe.map (\(Voice voice) -> voice.pitch == pitch)
        |> Maybe.withDefault False


getVoiceAt : Int -> Voicing -> Maybe Voice
getVoiceAt index (Voicing details) =
    List.Extra.getAt index details.voices


type alias VoiceCompare =
    { voiceA : VoiceDetails
    , voiceB : VoiceDetails
    }


toString : Voicing -> String
toString (Voicing details) =
    details.voices
        |> List.map voiceToString
        |> String.join ","


voiceToString : Voice -> String
voiceToString (Voice details) =
    Pitch.toString details.pitch


type Voicing
    = Voicing Internals


type alias Internals =
    { voices : List Voice
    }


type Voice
    = Voice VoiceDetails


type alias VoiceDetails =
    { pitch : Pitch.Pitch
    , interval : Interval.Interval
    , sourceInterval : Interval.Interval
    }
