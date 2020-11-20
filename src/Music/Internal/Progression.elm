module Music.Internal.Progression exposing
    ( Progression
    , ProgressionEvent
    , ProgressionType
    , ProgressionTypeEvent
    , chord
    , chords
    , fromProgressionType
    , join
    , keys
    , modulate
    , progressionType
    )

import Music.Internal.Analysis as Analysis
import Music.Internal.Chord as Chord
import Music.Internal.Duration as Duration
import Music.Internal.Key as Key
import Music.Internal.Modulation as Modulation


type Progression
    = Progression (List ProgressionEvent)


addEvent : ProgressionEvent -> Progression -> Progression
addEvent newEvent (Progression theEvents) =
    Progression (theEvents ++ [ newEvent ])


type ProgressionEvent
    = ProgressionEventChord Chord.Chord Duration.Duration
    | ProgressionEventKey Key.Key


type ProgressionType
    = ProgressionType (List ProgressionTypeEvent)


type ProgressionTypeEvent
    = ProgressionTypeEventChord Analysis.Analysis Duration.Duration
    | ProgressionTypeEventModulation Modulation.Modulation


fromProgressionType :
    Key.Key
    -> Analysis.DefaultChordTypes
    -> Modulation.NormalizeSettings
    -> ProgressionType
    -> Progression
fromProgressionType key defaultChordTypes normalizeSettings (ProgressionType typeEvents) =
    List.foldl
        (fromProgressionTypeStep defaultChordTypes normalizeSettings)
        { currentKey = key
        , progressionSoFar = Progression [ ProgressionEventKey key ]
        }
        typeEvents
        |> .progressionSoFar


fromProgressionTypeStep :
    Analysis.DefaultChordTypes
    -> Modulation.NormalizeSettings
    -> ProgressionTypeEvent
    -> { currentKey : Key.Key, progressionSoFar : Progression }
    -> { currentKey : Key.Key, progressionSoFar : Progression }
fromProgressionTypeStep defaultChordTypes normalizeSettings newEvent { currentKey, progressionSoFar } =
    case newEvent of
        ProgressionTypeEventChord analysis duration ->
            { currentKey = currentKey
            , progressionSoFar =
                let
                    newEventForOutput =
                        ProgressionEventChord
                            (Analysis.toChord defaultChordTypes
                                currentKey
                                analysis
                            )
                            duration
                in
                progressionSoFar
                    |> addEvent newEventForOutput
            }

        ProgressionTypeEventModulation modulation ->
            let
                newKey =
                    Modulation.apply
                        normalizeSettings
                        modulation
                        currentKey
            in
            { currentKey = newKey
            , progressionSoFar =
                let
                    newEventForOutput =
                        ProgressionEventKey newKey
                in
                progressionSoFar
                    |> addEvent newEventForOutput
            }


chords : Progression -> List Chord.Chord
chords (Progression theEvents) =
    theEvents
        |> List.filterMap
            (\e ->
                case e of
                    ProgressionEventChord theChord duration ->
                        Just theChord

                    ProgressionEventKey mod ->
                        Nothing
            )


keys : Progression -> List Key.Key
keys (Progression theEvents) =
    theEvents
        |> List.filterMap
            (\e ->
                case e of
                    ProgressionEventChord _ _ ->
                        Nothing

                    ProgressionEventKey key ->
                        Just key
            )


progressionType : List ProgressionTypeEvent -> ProgressionType
progressionType theProgressionTypeEvents =
    ProgressionType theProgressionTypeEvents


chord : Analysis.Analysis -> Duration.Duration -> ProgressionTypeEvent
chord analysis duration =
    ProgressionTypeEventChord analysis duration


modulate : Modulation.Modulation -> ProgressionTypeEvent
modulate modulation =
    ProgressionTypeEventModulation modulation


join : List ProgressionType -> ProgressionType
join progressions =
    List.concatMap events progressions
        |> ProgressionType


events : ProgressionType -> List ProgressionTypeEvent
events (ProgressionType theEvents) =
    theEvents
