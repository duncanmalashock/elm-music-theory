module MusicTheory.Sequence exposing
    ( NoteEvent
    , Sequence
    , appendNote
    , appendNotes
    , init
    , sequence1
    , sequence2
    , sequence3
    , toEvents
    )

import MusicTheory.Note as Note
import MusicTheory.Pitch as Pitch
import MusicTheory.Time as Time


type Entry
    = EntryNote NoteEntry
    | EntryTuplet TupletEntry
    | EntryRest RestEntry


type alias NoteEntry =
    { startTime : Time.Time
    , note : Note.Note
    }


type alias Tuplet =
    { duration : Time.Time
    , entries : List Entry
    }


type alias TupletEntry =
    { startTime : Time.Time
    , tuplet : Tuplet
    }


type alias RestEntry =
    { startTime : Time.Time
    , duration : Time.Time
    }


type Sequence
    = Sequence (List Entry)


sequence1 : Sequence
sequence1 =
    let
        theTuplet =
            init
                |> appendNote (Note.quarter Pitch.c3)
                |> appendNote (Note.quarter Pitch.d3)
                |> appendNote (Note.quarter Pitch.e3)
                |> appendNote (Note.quarter Pitch.f3)
                |> appendNote (Note.eighth Pitch.g3)
                |> appendNote (Note.eighth Pitch.a3)
                |> appendNote (Note.eighth Pitch.b3)
                |> appendNote (Note.eighth Pitch.c4)
                |> appendNote (Note.eighth Pitch.d4)
                |> appendNote (Note.quarter Pitch.e4)
                |> appendNote (Note.quarter Pitch.f4)
                |> appendNote (Note.eighth Pitch.g4)
                |> appendNote (Note.eighth Pitch.a4)
                |> appendNote (Note.eighth Pitch.b4)
                |> appendNote (Note.eighth Pitch.c5)
                |> appendNote (Note.eighth Pitch.d5)
                |> tuplet
                    { duration =
                        Time.whole
                            |> Time.add Time.whole
                            |> Time.add Time.whole
                    }
    in
    init
        |> appendTuplet theTuplet


sequence2 : Sequence
sequence2 =
    let
        theTuplet =
            init
                |> appendNote (Note.eighth Pitch.e4)
                |> appendNote (Note.eighth Pitch.d4)
                |> appendNote (Note.sixteenth Pitch.c4)
                |> appendNote (Note.eighth Pitch.b3)
                |> appendNote (Note.eighth Pitch.a3)
                |> appendNote (Note.eighth Pitch.g3)
                |> appendNote (Note.sixteenth Pitch.f3)
                |> appendNote (Note.eighth Pitch.e3)
                |> appendNote (Note.eighth Pitch.d3)
                |> appendNote (Note.eighth Pitch.c3)
                |> appendNote (Note.eighth Pitch.d3)
                |> appendNote (Note.sixteenth Pitch.e3)
                |> appendNote (Note.eighth Pitch.f3)
                |> appendNote (Note.eighth Pitch.g3)
                |> appendNote (Note.eighth Pitch.a3)
                |> appendNote (Note.sixteenth Pitch.b3)
                |> appendNote (Note.eighth Pitch.c4)
                |> appendNote (Note.sixteenth Pitch.d4)
                |> appendNote (Note.eighth Pitch.e4)
                |> appendNote (Note.eighth Pitch.d4)
                |> appendNote (Note.eighth Pitch.c4)
                |> appendNote (Note.sixteenth Pitch.b3)
                |> appendNote (Note.sixteenth Pitch.a3)
                |> appendNote (Note.eighth Pitch.g3)
                |> appendNote (Note.eighth Pitch.f3)
                |> tuplet
                    { duration =
                        Time.whole
                            |> Time.add Time.whole
                            |> Time.add Time.whole
                    }
    in
    init
        |> appendTuplet theTuplet


sequence3 : Sequence
sequence3 =
    init
        |> appendNote (Note.eighth Pitch.e5)
        |> appendNote (Note.eighth Pitch.d5)
        |> appendNote (Note.sixteenth Pitch.c5)
        |> appendNote (Note.eighth Pitch.b4)
        |> appendNote (Note.eighth Pitch.a4)
        |> appendNote (Note.eighth Pitch.g4)
        |> appendNote (Note.sixteenth Pitch.f4)
        |> appendNote (Note.eighth Pitch.e4)
        |> appendNote (Note.eighth Pitch.d4)
        |> appendNote (Note.eighth Pitch.c4)
        |> appendNote (Note.eighth Pitch.d4)
        |> appendNote (Note.sixteenth Pitch.e4)
        |> appendNote (Note.eighth Pitch.f4)
        |> appendNote (Note.eighth Pitch.g4)
        |> appendNote (Note.eighth Pitch.a4)
        |> appendNote (Note.sixteenth Pitch.b4)
        |> appendNote (Note.eighth Pitch.c5)
        |> appendNote (Note.sixteenth Pitch.d5)
        |> appendNote (Note.eighth Pitch.e5)
        |> appendNote (Note.eighth Pitch.d5)
        |> appendNote (Note.eighth Pitch.c5)
        |> appendNote (Note.sixteenth Pitch.b4)
        |> appendNote (Note.eighth Pitch.c5)
        |> appendNote (Note.sixteenth Pitch.d5)
        |> appendNote (Note.eighth Pitch.e5)
        |> appendNote (Note.eighth Pitch.d5)
        |> appendNote (Note.eighth Pitch.c5)


appendTuplet : Tuplet -> Sequence -> Sequence
appendTuplet theTuplet theSequence =
    let
        newEntryAtTime =
            EntryTuplet
                { startTime = firstAvailableStart theSequence
                , tuplet = theTuplet
                }
    in
    sequence <|
        getEntries theSequence
            ++ [ newEntryAtTime ]


tuplet :
    { duration : Time.Time
    }
    -> Sequence
    -> Tuplet
tuplet { duration } theSequence =
    { duration = duration
    , entries = normalizeStartTimes (getEntries theSequence)
    }


init : Sequence
init =
    Sequence []


appendNote : Note.Note -> Sequence -> Sequence
appendNote note theSequence =
    let
        newEntryAtTime =
            EntryNote
                { startTime = firstAvailableStart theSequence
                , note = note
                }
    in
    sequence <|
        getEntries theSequence
            ++ [ newEntryAtTime ]


appendRest : Time.Time -> Sequence -> Sequence
appendRest restDuration theSequence =
    let
        newEntryAtTime =
            EntryRest
                { startTime = firstAvailableStart theSequence
                , duration = restDuration
                }
    in
    sequence <|
        getEntries theSequence
            ++ [ newEntryAtTime ]


appendNotes : List Note.Note -> Sequence -> Sequence
appendNotes notes theSequence =
    List.foldl appendNote theSequence notes


sequence : List Entry -> Sequence
sequence theEntries =
    Sequence theEntries


type alias NoteEvent =
    { time : Float
    , pitch : Int
    , duration : Float
    }


sortEntries : List Entry -> List Entry
sortEntries entries =
    List.sortWith orderEntries entries


orderEntries : Entry -> Entry -> Order
orderEntries entryA entryB =
    compare
        (Time.toFloat <| getStartTime entryA)
        (Time.toFloat <| getStartTime entryB)


normalizeStartTimes : List Entry -> List Entry
normalizeStartTimes entries =
    let
        startTimeOffset =
            sortEntries entries
                |> List.head
                |> Maybe.map getStartTime
                |> Maybe.withDefault Time.zero
    in
    List.map (addStartTime startTimeOffset) entries


totalDuration : List NoteEntry -> Time.Time
totalDuration entries =
    let
        addDurationToStartTime : NoteEntry -> Time.Time
        addDurationToStartTime note =
            Time.add (Note.duration note.note) note.startTime
    in
    entries
        |> List.reverse
        |> List.head
        |> Maybe.map addDurationToStartTime
        |> Maybe.withDefault Time.zero


addStartTime : Time.Time -> Entry -> Entry
addStartTime timeToAdd entry =
    case entry of
        EntryNote { startTime, note } ->
            EntryNote
                { startTime = Time.add timeToAdd startTime
                , note = note
                }

        EntryTuplet tupletEntry ->
            EntryTuplet
                { startTime = Time.add timeToAdd tupletEntry.startTime
                , tuplet = tupletEntry.tuplet
                }

        EntryRest { startTime, duration } ->
            EntryRest
                { startTime = Time.add timeToAdd startTime
                , duration = duration
                }


addStartTimeToNoteEntry : Time.Time -> NoteEntry -> NoteEntry
addStartTimeToNoteEntry timeToAdd entry =
    { startTime = Time.add timeToAdd entry.startTime
    , note = entry.note
    }


getEntries : Sequence -> List Entry
getEntries (Sequence theEntries) =
    theEntries


toEvents : Int -> Sequence -> List NoteEvent
toEvents tempo theSequence =
    let
        tempoAsCoefficient =
            4 / (Basics.toFloat tempo / 60)
    in
    getEntries theSequence
        |> flattenToNoteEntries
        |> noteEntriesToNoteEvents tempoAsCoefficient


flattenToNoteEntries : List Entry -> List NoteEntry
flattenToNoteEntries entriesToConvert =
    entriesToConvert
        |> List.concatMap
            (\entry ->
                case entry of
                    EntryNote noteEntry ->
                        [ noteEntry ]

                    EntryTuplet tupletEntry ->
                        tupletEntry.tuplet.entries
                            |> flattenToNoteEntries
                            |> stretchToDuration tupletEntry.tuplet.duration
                            |> List.map (addStartTimeToNoteEntry tupletEntry.startTime)

                    EntryRest { startTime, duration } ->
                        []
            )


stretchToDuration : Time.Time -> List NoteEntry -> List NoteEntry
stretchToDuration duration noteEntries =
    let
        ratio : Time.Time
        ratio =
            Time.divide duration (totalDuration noteEntries)

        stretch : Time.Time -> NoteEntry -> NoteEntry
        stretch ratioToStretch noteEntry =
            { startTime = Time.multiply noteEntry.startTime ratioToStretch
            , note = Note.multiplyDuration noteEntry.note ratioToStretch
            }
    in
    noteEntries
        |> List.map (stretch ratio)


noteEntriesToNoteEvents : Float -> List NoteEntry -> List NoteEvent
noteEntriesToNoteEvents tempoAsCoefficient entryList =
    entryList
        |> List.map
            (\{ startTime, note } ->
                { time =
                    startTime
                        |> Time.toFloat
                        |> (*) tempoAsCoefficient
                , pitch = Note.toMidiNote note
                , duration =
                    Note.duration note
                        |> Time.toFloat
                        |> (*) tempoAsCoefficient
                }
            )


getStartTime : Entry -> Time.Time
getStartTime entry =
    case entry of
        EntryNote noteEntry ->
            noteEntry.startTime

        EntryTuplet tupletEntry ->
            tupletEntry.startTime

        EntryRest restEntry ->
            restEntry.startTime


getDuration : Entry -> Time.Time
getDuration entry =
    case entry of
        EntryNote noteEntry ->
            Note.duration noteEntry.note

        EntryTuplet tupletEntry ->
            tupletEntry.tuplet.duration

        EntryRest restEntry ->
            restEntry.duration


firstAvailableStart : Sequence -> Time.Time
firstAvailableStart theSequence =
    getEntries theSequence
        |> List.reverse
        |> List.head
        |> Maybe.map
            (\entry ->
                Time.add (getStartTime entry) (getDuration entry)
            )
        |> Maybe.withDefault Time.zero
