module Music exposing (Music, new)

{-|

@docs Music, new

-}

import Music.Chord exposing (Chord)
import Music.Duration as Duration exposing (Duration)
import Music.Key as Key exposing (Key)
import Music.Meter as Meter exposing (Meter)
import Music.Note as Note exposing (Note)
import Music.Range as Range
import Music.Tempo as Tempo exposing (Tempo)


type Music
    = Music Details


type alias Details =
    { tempoEvents : List (Event Tempo)
    , keyEvents : List (Event Key)
    , meterEvents : List (Event Meter)
    , chordEvents : List (Event Chord)
    , noteEvents : List (Event Note)
    , instruments : List Instrument
    }


new :
    { tempo : Tempo.Tempo
    , key : Key.Key
    , meter : Meter.Meter
    }
    -> Music
new { tempo, key, meter } =
    Music
        { tempoEvents =
            [ event Duration.zero tempo
            ]
        , keyEvents =
            [ event Duration.zero key
            ]
        , meterEvents =
            [ event Duration.zero meter
            ]
        , chordEvents = []
        , noteEvents = []
        , instruments = []
        }


type Event a
    = Event
        { at : Duration
        , value : a
        }


type alias Instrument =
    { name : String
    , range : Range.Range
    }


addNote :
    { note : Note.Note
    , at : Duration
    }
    -> Music
    -> Music
addNote options (Music music) =
    let
        newNoteEvent : Event Note.Note
        newNoteEvent =
            event options.at options.note
    in
    Music
        { music
            | noteEvents =
                newNoteEvent :: music.noteEvents
        }


event : Duration -> a -> Event a
event at value =
    Event
        { at = at
        , value = value
        }
