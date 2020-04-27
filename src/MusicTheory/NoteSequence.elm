module MusicTheory.NoteSequence exposing
    ( NoteEvent
    , NoteSequence
    , appendNote
    , appendNotes
    , init
    , sequenceWithTriplets
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


type NoteSequence
    = NoteSequence (List Entry)


sequenceWithTriplets : NoteSequence
sequenceWithTriplets =
    let
        triplet1 =
            init
                |> appendNote (Note.sixteenth Pitch.d5)
                |> appendNote (Note.sixteenth Pitch.c5)
                |> appendNote (Note.sixteenth Pitch.b4)
                |> tuplet
                    { duration = Time.eighth
                    }

        triplet2 =
            init
                |> appendNote (Note.eighth Pitch.c5)
                |> appendNote (Note.eighth Pitch.b4)
                |> appendNote (Note.eighth Pitch.a4)
                |> tuplet
                    { duration = Time.quarter
                    }
    in
    init
        |> appendNote (Note.quarter Pitch.b4)
        |> appendRest Time.eighth
        |> appendNote (Note.sixteenth Pitch.b4)
        |> appendNote (Note.sixteenth Pitch.b4)
        |> appendNote (Note.eighth Pitch.b4)
        |> appendNote (Note.eighth Pitch.cSharp5)
        |> appendNote (Note.eighth Pitch.d5)
        |> appendNote (Note.eighth Pitch.e5)
        |> appendNote (Note.quarter Pitch.cSharp5)
        |> appendNote (Note.half Pitch.a4 |> Note.dotted)
        |> appendNote (Note.quarter Pitch.d5)
        |> appendNote (Note.sixteenth Pitch.d5)
        |> appendNote (Note.sixteenth Pitch.e5)
        |> appendTuplet triplet1
        |> appendTuplet triplet2
        |> appendNote (Note.eighth Pitch.g4)
        |> appendNote (Note.eighth Pitch.d4)
        |> appendNote (Note.whole Pitch.e4)


appendTuplet : Tuplet -> NoteSequence -> NoteSequence
appendTuplet theTuplet sequence =
    let
        newEntryAtTime =
            EntryTuplet
                { startTime = firstAvailableStart sequence
                , tuplet = theTuplet
                }
    in
    noteSequence <|
        getEntries sequence
            ++ [ newEntryAtTime ]


tuplet :
    { duration : Time.Time
    }
    -> NoteSequence
    -> Tuplet
tuplet { duration } sequence =
    { duration = duration
    , entries = normalizeStartTimes (getEntries sequence)
    }


init : NoteSequence
init =
    NoteSequence []


appendNote : Note.Note -> NoteSequence -> NoteSequence
appendNote note sequence =
    let
        newEntryAtTime =
            EntryNote
                { startTime = firstAvailableStart sequence
                , note = note
                }
    in
    noteSequence <|
        getEntries sequence
            ++ [ newEntryAtTime ]


appendRest : Time.Time -> NoteSequence -> NoteSequence
appendRest restDuration sequence =
    let
        newEntryAtTime =
            EntryRest
                { startTime = firstAvailableStart sequence
                , duration = restDuration
                }
    in
    noteSequence <|
        getEntries sequence
            ++ [ newEntryAtTime ]


appendNotes : List Note.Note -> NoteSequence -> NoteSequence
appendNotes notes sequence =
    List.foldl appendNote sequence notes


noteSequence : List Entry -> NoteSequence
noteSequence theEntries =
    NoteSequence theEntries


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


getEntries : NoteSequence -> List Entry
getEntries (NoteSequence theEntries) =
    theEntries


toEvents : Int -> NoteSequence -> List NoteEvent
toEvents tempo sequence =
    let
        tempoAsCoefficient =
            4 / (Basics.toFloat tempo / 60)
    in
    getEntries sequence
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


firstAvailableStart : NoteSequence -> Time.Time
firstAvailableStart sequence =
    getEntries sequence
        |> List.reverse
        |> List.head
        |> Maybe.map
            (\entry ->
                Time.add (getStartTime entry) (getDuration entry)
            )
        |> Maybe.withDefault Time.zero
