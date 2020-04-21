module MusicTheory.NoteSequence exposing
    ( NoteEvent
    , NoteSequence
    , appendNote
    , appendNotes
    , init
    , toEvents
    )

import MusicTheory.Note as Note
import MusicTheory.Time as Time
import MusicTheory.Tuplet as Tuplet


type Entry
    = Single Time.Time Note.Note
      --| Tuplet Time.Time Tuplet.Tuplet
    | Rest Time.Time Time.Time


type alias NoteSequence =
    { noteEntries : List Entry
    }


type alias NoteEvent =
    { time : Float
    , pitch : Int
    , duration : Float
    }


toEvents : Int -> NoteSequence -> List NoteEvent
toEvents tempo noteSequence =
    let
        tempoAsCoefficient =
            4 / (Basics.toFloat tempo / 60)
    in
    noteSequence.noteEntries
        |> List.filterMap
            (\entry ->
                case entry of
                    Single _ note ->
                        Just
                            { time =
                                entryStartTime entry
                                    |> Time.toFloat
                                    |> (*) tempoAsCoefficient
                            , pitch = Note.toMidiNote note
                            , duration =
                                entryDuration entry
                                    |> Time.toFloat
                                    |> (*) tempoAsCoefficient
                            }

                    Rest _ _ ->
                        Nothing
            )


init : NoteSequence
init =
    { noteEntries = []
    }


entryStartTime : Entry -> Time.Time
entryStartTime entry =
    case entry of
        Single time note ->
            time

        Rest time duration ->
            time


entryDuration : Entry -> Time.Time
entryDuration entry =
    case entry of
        Single time note ->
            Note.duration note

        Rest time duration ->
            duration


firstAvailableStart : NoteSequence -> Time.Time
firstAvailableStart sequence =
    sequence.noteEntries
        |> List.reverse
        |> List.head
        |> Maybe.map
            (\entry ->
                Time.add (entryStartTime entry) (entryDuration entry)
            )
        |> Maybe.withDefault Time.zero


appendNote : Note.Note -> NoteSequence -> NoteSequence
appendNote note sequence =
    let
        newEntryAtTime =
            Single (firstAvailableStart sequence) note
    in
    { sequence
        | noteEntries =
            sequence.noteEntries ++ [ newEntryAtTime ]
    }


appendNotes : List Note.Note -> NoteSequence -> NoteSequence
appendNotes notes sequence =
    List.foldl appendNote sequence notes
