module MusicTheory.NoteSequence exposing (NoteSequence)

import MusicTheory.Note as Note
import MusicTheory.Time as Time
import MusicTheory.Tuplet as Tuplet


type NoteGrouping
    = Single Note.Note
    | Tuplet Tuplet.Tuplet


type NoteEntry
    = NoteEntry Time.Time NoteGrouping


type alias NoteSequence =
    { noteEntries : List NoteEntry
    }
