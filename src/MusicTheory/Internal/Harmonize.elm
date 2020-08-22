module MusicTheory.Internal.Harmonize exposing
    ( HarmonizedContext
    , Tone
    , chordFromHarmonizedContext
    , chordTone
    , diatonicApproach
    , diminishedApproach
    , dominantApproach
    , execute
    , nonChordTone
    , nonScaleTone
    , parallelApproach
    , toneFromHarmonicContext
    )

import MusicTheory.Internal.Chord as Chord
import MusicTheory.Internal.ChordScale as ChordScale
import MusicTheory.Internal.ChordType as ChordType
import MusicTheory.Internal.HarmonicContext as HarmonicContext
import MusicTheory.Internal.Interval as Interval
import MusicTheory.Internal.Pitch as Pitch
import MusicTheory.Internal.PitchClass as PitchClass
import MusicTheory.Internal.Scale as Scale
import Util.Basic


type Tone
    = ChordTone
    | NonChordTone
    | NonScaleTone


chordTone : Tone
chordTone =
    ChordTone


nonChordTone : Tone
nonChordTone =
    NonChordTone


nonScaleTone : Tone
nonScaleTone =
    NonScaleTone


toneFromHarmonicContext : HarmonicContext.HarmonicContext -> Tone
toneFromHarmonicContext context =
    let
        isChordTone =
            Chord.containsPitchClass
                (HarmonicContext.pitch context |> Pitch.pitchClass)
                (HarmonicContext.chord context)

        isScaleTone =
            Scale.containsPitchClass
                (HarmonicContext.pitch context |> Pitch.pitchClass)
                (HarmonicContext.scale context)
                { ignoreSpelling = True }
    in
    if isChordTone then
        ChordTone

    else if isScaleTone then
        NonChordTone

    else
        NonScaleTone


type alias HarmonizedContext =
    { context : HarmonicContext.HarmonicContext
    , maybeChord : Maybe Chord.Chord
    }


execute :
    { ifNonChordTone :
        HarmonizedContext
        -> Maybe HarmonizedContext
        -> Maybe Chord.Chord
    , ifNonScaleTone :
        HarmonizedContext
        -> Maybe HarmonizedContext
        -> Maybe Chord.Chord
    }
    -> List HarmonicContext.HarmonicContext
    -> List HarmonizedContext
execute tactics contexts =
    contexts
        |> List.map initialStep
        -- Apply this until all chords are harmonized (bailing out after 10 attempts)
        |> Util.Basic.applyNTimes
            10
            (listMapPairs
                (\context maybeNextContext ->
                    step tactics context maybeNextContext
                )
            )


chordFromHarmonizedContext : HarmonizedContext -> Maybe Chord.Chord
chordFromHarmonizedContext harmonizedContext =
    harmonizedContext.maybeChord


listMapPairs : (a -> Maybe a -> b) -> List a -> List b
listMapPairs fn list =
    listToTuples list []
        |> List.map
            (\( item, maybeNextItem ) ->
                fn item maybeNextItem
            )


listToTuples : List a -> List ( a, Maybe a ) -> List ( a, Maybe a )
listToTuples theList listSoFar =
    case theList of
        [] ->
            listSoFar

        head :: tail ->
            listToTuples tail (listSoFar ++ [ ( head, List.head tail ) ])


initialStep :
    HarmonicContext.HarmonicContext
    -> HarmonizedContext
initialStep context =
    case toneFromHarmonicContext context of
        ChordTone ->
            { context =
                context
            , maybeChord =
                HarmonicContext.chord context
                    |> Just
            }

        NonChordTone ->
            { context =
                context
            , maybeChord =
                Nothing
            }

        NonScaleTone ->
            { context =
                context
            , maybeChord =
                Nothing
            }


step :
    { ifNonChordTone :
        HarmonizedContext
        -> Maybe HarmonizedContext
        -> Maybe Chord.Chord
    , ifNonScaleTone :
        HarmonizedContext
        -> Maybe HarmonizedContext
        -> Maybe Chord.Chord
    }
    -> HarmonizedContext
    -> Maybe HarmonizedContext
    -> HarmonizedContext
step { ifNonChordTone, ifNonScaleTone } harmonizedContext maybeNext =
    case toneFromHarmonicContext harmonizedContext.context of
        ChordTone ->
            harmonizedContext

        NonChordTone ->
            { context =
                harmonizedContext.context
            , maybeChord =
                ifNonChordTone harmonizedContext maybeNext
            }

        NonScaleTone ->
            { context =
                harmonizedContext.context
            , maybeChord =
                ifNonScaleTone harmonizedContext maybeNext
            }


diatonicApproach :
    List ChordType.ChordType
    -> HarmonizedContext
    -> Maybe HarmonizedContext
    -> Maybe Chord.Chord
diatonicApproach chordTypesAllowed harmonized maybeNext =
    -- 1. get the interval between the pitch of the current and next context's pitch
    -- 2. get the root of the next harmonized context's chord if it exists
    -- 3. transpose the root of that chord down by the interval in step 1
    -- 4. harmonize with a chord class found in the given scale at that root
    maybeNext
        |> Maybe.andThen
            (\nextContext ->
                nextContext.maybeChord
                    |> Maybe.andThen
                        (\nextChord ->
                            let
                                intervalBetweenPitches =
                                    Pitch.intervalBetween
                                        (HarmonicContext.pitch nextContext.context)
                                        (HarmonicContext.pitch harmonized.context)

                                newRoot =
                                    PitchClass.transpose
                                        intervalBetweenPitches
                                        (Chord.root nextChord)
                            in
                            ChordScale.diatonicChordsAt
                                { root = newRoot
                                , scale = HarmonicContext.scale harmonized.context
                                , chordTypesAllowed = chordTypesAllowed
                                }
                                |> List.head
                        )
            )


parallelApproach :
    HarmonizedContext
    -> Maybe HarmonizedContext
    -> Maybe Chord.Chord
parallelApproach harmonized maybeNext =
    -- 1. get the interval between the pitch of the current and next context's pitch
    -- 2. get the root of the next harmonized context's chord if it exists
    -- 3. transpose the root of that chord down by the interval in step 1
    -- 4. harmonize with the same chord class as the next chord
    maybeNext
        |> Maybe.andThen
            (\nextContext ->
                nextContext.maybeChord
                    |> Maybe.andThen
                        (\nextChord ->
                            let
                                intervalBetweenPitches =
                                    Pitch.intervalBetween
                                        (HarmonicContext.pitch nextContext.context)
                                        (HarmonicContext.pitch harmonized.context)

                                newRoot =
                                    PitchClass.transpose
                                        intervalBetweenPitches
                                        (Chord.root nextChord)
                            in
                            HarmonicContext.chord nextContext.context
                                |> Chord.chordType
                                |> Chord.chord newRoot
                                |> Just
                        )
            )


diminishedApproach :
    HarmonizedContext
    -> Maybe HarmonizedContext
    -> Maybe Chord.Chord
diminishedApproach harmonized maybeNext =
    -- 1. get the interval between the pitch of the current and next context's pitch
    -- 2. get the root of the next harmonized context's chord if it exists
    -- 3. transpose the root of that chord down by the interval in step 1
    -- 4. harmonize with a diminished seventh chord
    maybeNext
        |> Maybe.andThen
            (\nextContext ->
                nextContext.maybeChord
                    |> Maybe.andThen
                        (\nextChord ->
                            let
                                intervalBetweenPitches =
                                    Pitch.intervalBetween
                                        (HarmonicContext.pitch nextContext.context)
                                        (HarmonicContext.pitch harmonized.context)

                                newRoot =
                                    PitchClass.transpose
                                        intervalBetweenPitches
                                        (Chord.root nextChord)
                            in
                            Chord.chord newRoot
                                ChordType.diminishedSeventh
                                |> Just
                        )
            )


dominantApproach :
    List ChordType.ChordType
    -> HarmonizedContext
    -> Maybe HarmonizedContext
    -> Maybe Chord.Chord
dominantApproach chordTypesAllowed harmonized maybeNext =
    -- 1. get the interval between the pitch of the current and next context's pitch
    -- 2. get the root of the next harmonized context's chord if it exists
    -- 3. transpose the root of that chord down by a fifth
    -- 4. harmonize with all dominant chord classes supplied
    -- 5. filter by only the chords which include the melody note of the precending context
    maybeNext
        |> Maybe.andThen
            (\nextContext ->
                nextContext.maybeChord
                    |> Maybe.andThen
                        (\nextChord ->
                            let
                                newRoot =
                                    PitchClass.transpose
                                        Interval.perfectFifth
                                        (Chord.root nextChord)
                            in
                            List.map
                                (Chord.chord newRoot)
                                chordTypesAllowed
                                |> List.filter
                                    (\chord ->
                                        Chord.containsPitchClass
                                            (HarmonicContext.pitch harmonized.context
                                                |> Pitch.pitchClass
                                            )
                                            chord
                                    )
                                |> List.head
                        )
            )
