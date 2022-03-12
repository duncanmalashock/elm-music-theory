module Internal.Music exposing (Music, new)

{-|

@docs Music, new

-}

import Internal.Chord exposing (Chord)
import Internal.Duration as Duration exposing (Duration)
import Internal.Key as Key exposing (Key)
import Internal.Meter as Meter exposing (Meter)
import Internal.Note as Note
import Internal.Pitch as Pitch
import Internal.Tempo as Tempo exposing (Tempo)


type Music
    = Music Details


type alias Id =
    Int


type Event a
    = Event
        { at : Duration
        , id : Id
        , value : a
        }


type alias Details =
    { tempoEvents : List (Event Tempo)
    , keyEvents : List (Event Key)
    , meterEvents : List (Event Meter)
    , chordEvents : List (Event Chord)
    , noteEvents : List (Event Note)
    , instruments : List Instrument
    }


type alias Note =
    { note : Note.Note
    , instrumentId : Id
    }


type alias Instrument =
    { id : Id
    , name : String
    , range : Pitch.Range
    }


event : Duration -> a -> Id -> Event a
event at value id =
    Event
        { at = at
        , id = id
        , value = value
        }


new : Music
new =
    Music
        { tempoEvents =
            [ event Duration.zero (Tempo.quarterNotesPerMinute 60) 0
            ]
        , keyEvents =
            [ event Duration.zero Key.c 0
            ]
        , meterEvents =
            [ event Duration.zero Meter.fourFour 0
            ]
        , chordEvents = []
        , noteEvents = []
        , instruments = []
        }
