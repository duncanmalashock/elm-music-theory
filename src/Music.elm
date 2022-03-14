module Music exposing
    ( Music, new
    , addNote, removeNote
    , NoteEvent, noteEvents
    )

{-|

@docs Music, new

@docs addNote, removeNote

@docs NoteEvent noteEvents

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


type alias Event a =
    { at : Duration
    , value : a
    }


type alias NoteEvent =
    { at : Duration
    , value : Note
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


removeNote :
    { note : Note.Note
    , at : Duration
    }
    -> Music
    -> Music
removeNote options (Music music) =
    let
        matches : Event Note.Note -> Bool
        matches current =
            (current.value == options.note)
                && (current.at == options.at)
    in
    Music
        { music
            | noteEvents =
                List.filter (\current -> not (matches current)) music.noteEvents
        }


event : Duration -> a -> Event a
event at value =
    { at = at
    , value = value
    }


noteEvents : Music -> List NoteEvent
noteEvents (Music music) =
    music.noteEvents
