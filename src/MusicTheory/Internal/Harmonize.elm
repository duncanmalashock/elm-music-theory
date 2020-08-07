module MusicTheory.Internal.Harmonize exposing
    ( Tone
    , chordTone
    , nonChordTone
    , nonScaleTone
    , toneFromHarmonicContext
    )

import MusicTheory.Internal.Chord as Chord
import MusicTheory.Internal.ChordScale as ChordScale
import MusicTheory.Internal.HarmonicContext as HarmonicContext
import MusicTheory.Internal.Pitch as Pitch
import MusicTheory.Internal.Scale as Scale


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


step :
    { ifNonChordTone :
        HarmonicContext.HarmonicContext
        -> Maybe HarmonicContext.HarmonicContext
        -> Maybe Chord.Chord
    , ifNonScaleTone :
        HarmonicContext.HarmonicContext
        -> Maybe HarmonicContext.HarmonicContext
        -> Maybe Chord.Chord
    }
    -> HarmonicContext.HarmonicContext
    -> Maybe HarmonicContext.HarmonicContext
    -> Maybe Chord.Chord
step { ifNonChordTone, ifNonScaleTone } context maybeNextContext =
    case toneFromHarmonicContext context of
        ChordTone ->
            HarmonicContext.chord context
                |> Just

        NonChordTone ->
            ifNonChordTone context maybeNextContext

        NonScaleTone ->
            ifNonScaleTone context maybeNextContext


diatonicApproach :
    HarmonicContext.HarmonicContext
    -> Maybe HarmonicContext.HarmonicContext
    -> Maybe Chord.Chord
diatonicApproach context maybeNextContext =
    ChordScale.diatonicChordsAt
        { root = Chord.root (HarmonicContext.chord context)
        , scale = HarmonicContext.scale context
        }
        -- TODO: don't just pick the first one; sort the list or pick one intelligently
        |> List.head


parallelApproach :
    HarmonicContext.HarmonicContext
    -> Maybe HarmonicContext.HarmonicContext
    -> Maybe Chord.Chord
parallelApproach context maybeNextContext =
    Maybe.map
        (\nextContext ->
            HarmonicContext.chord nextContext
                |> Chord.chordClass
                |> Chord.chord (HarmonicContext.pitchClass context)
        )
        maybeNextContext
