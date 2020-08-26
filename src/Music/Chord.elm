module Music.Chord exposing
    ( Chord
    , chordType, root, containsPitchClass
    , toPitchClasses, symbol
    , major, minor, augmented, diminished, sus2, sus4
    , majorSix, majorSixNine, minorSix, minorSixNine, majorAddNine, minorAddNine
    , majorSeventh, majorSeventhSharpEleven, minorSeventh, dominantSeventh, diminishedSeventh, halfDiminished, augmentedDominantSeventh, dominantSeventhSus4, minorMajorSeventh
    , majorNinth, minorNinth, dominantNinth, minorEleventh, dominantEleventh, dominantThirteenth
    , dominantSeventhFlatNine, dominantSeventhSharpNine, dominantSeventhFlatNineSharpNine, dominantSeventhFlatNineSharpEleven, dominantSeventhSharpNineSharpEleven, dominantSeventhSharpEleven, dominantSeventhFlatNineFlatThirteen, dominantSeventhSharpNineFlatThirteen, dominantSeventhSharpElevenFlatThirteen
    , custom
    )

{-| A [chord](https://en.wikipedia.org/wiki/Chord_%28music%29) is a set of pitch classes that are sounded together to create harmony.

@docs Chord


# Helpers

@docs chordType, root, containsPitchClass


# Conversion

@docs toPitchClasses, symbol


# Constructors


## Triads

@docs major, minor, augmented, diminished, sus2, sus4


## Added-tone chords

@docs majorSix, majorSixNine, minorSix, minorSixNine, majorAddNine, minorAddNine


## Seventh chords

@docs majorSeventh, majorSeventhSharpEleven, minorSeventh, dominantSeventh, diminishedSeventh, halfDiminished, augmentedDominantSeventh, dominantSeventhSus4, minorMajorSeventh


## Chords with extensions

@docs majorNinth, minorNinth, dominantNinth, minorEleventh, dominantEleventh, dominantThirteenth


## Altered dominant chords

@docs dominantSeventhFlatNine, dominantSeventhSharpNine, dominantSeventhFlatNineSharpNine, dominantSeventhFlatNineSharpEleven, dominantSeventhSharpNineSharpEleven, dominantSeventhSharpEleven, dominantSeventhFlatNineFlatThirteen, dominantSeventhSharpNineFlatThirteen, dominantSeventhSharpElevenFlatThirteen


# Custom chord types

@docs custom

-}

import Music.Internal.Chord as Chord
import Music.Internal.ChordType as ChordType
import Music.Internal.PitchClass as PitchClass


{-| -}
type alias Chord =
    Chord.Chord


{-| Get a chord's chord type:

    chordType (dominantSeventh PitchClass.a) == dominantSeventh

-}
chordType : Chord -> ChordType.ChordType
chordType theChord =
    Chord.chordType theChord


{-| Get a chord's root:

    chordType (dominantSeventh PitchClass.a) == PitchClass.a

-}
root : Chord -> PitchClass.PitchClass
root theChord =
    Chord.root theChord


{-| Determine whether a chord contains a given pitch class:

    containsPitchClass PitchClass.cSharp (major PitchClass.a) == True

-}
containsPitchClass : PitchClass.PitchClass -> Chord -> Bool
containsPitchClass pitchClass theChord =
    Chord.containsPitchClass pitchClass theChord


{-| Get the pitch classes in a chord:

    toPitchClasses (major PitchClass.a) == [ PitchClass.a, PitchClass.cSharp, PitchClass.e ]

-}
toPitchClasses : Chord -> List PitchClass.PitchClass
toPitchClasses theChord =
    Chord.toPitchClasses theChord


{-| Get the chord symbol for a chord:

    symbol (dominantNinth PitchClass.a) == "A9"

-}
symbol : Chord -> String
symbol chord =
    Chord.symbol chord


{-| -}
major : PitchClass.PitchClass -> Chord
major pitchClass =
    Chord.chord pitchClass ChordType.major


{-| -}
minor : PitchClass.PitchClass -> Chord
minor pitchClass =
    Chord.chord pitchClass ChordType.minor


{-| -}
augmented : PitchClass.PitchClass -> Chord
augmented pitchClass =
    Chord.chord pitchClass ChordType.augmented


{-| -}
diminished : PitchClass.PitchClass -> Chord
diminished pitchClass =
    Chord.chord pitchClass ChordType.diminished


{-| -}
sus2 : PitchClass.PitchClass -> Chord
sus2 pitchClass =
    Chord.chord pitchClass ChordType.sus2


{-| -}
sus4 : PitchClass.PitchClass -> Chord
sus4 pitchClass =
    Chord.chord pitchClass ChordType.sus4


{-| -}
majorSix : PitchClass.PitchClass -> Chord
majorSix pitchClass =
    Chord.chord pitchClass ChordType.majorSix


{-| -}
majorSixNine : PitchClass.PitchClass -> Chord
majorSixNine pitchClass =
    Chord.chord pitchClass ChordType.majorSixNine


{-| -}
minorSix : PitchClass.PitchClass -> Chord
minorSix pitchClass =
    Chord.chord pitchClass ChordType.minorSix


{-| -}
minorSixNine : PitchClass.PitchClass -> Chord
minorSixNine pitchClass =
    Chord.chord pitchClass ChordType.minorSixNine


{-| -}
majorAddNine : PitchClass.PitchClass -> Chord
majorAddNine pitchClass =
    Chord.chord pitchClass ChordType.majorAddNine


{-| -}
minorAddNine : PitchClass.PitchClass -> Chord
minorAddNine pitchClass =
    Chord.chord pitchClass ChordType.minorAddNine


{-| -}
majorSeventh : PitchClass.PitchClass -> Chord
majorSeventh pitchClass =
    Chord.chord pitchClass ChordType.majorSeventh


{-| -}
minorSeventh : PitchClass.PitchClass -> Chord
minorSeventh pitchClass =
    Chord.chord pitchClass ChordType.minorSeventh


{-| -}
dominantSeventh : PitchClass.PitchClass -> Chord
dominantSeventh pitchClass =
    Chord.chord pitchClass ChordType.dominantSeventh


{-| -}
diminishedSeventh : PitchClass.PitchClass -> Chord
diminishedSeventh pitchClass =
    Chord.chord pitchClass ChordType.diminishedSeventh


{-| -}
halfDiminished : PitchClass.PitchClass -> Chord
halfDiminished pitchClass =
    Chord.chord pitchClass ChordType.halfDiminished


{-| -}
majorSeventhSharpEleven : PitchClass.PitchClass -> Chord
majorSeventhSharpEleven pitchClass =
    Chord.chord pitchClass ChordType.majorSeventhSharpEleven


{-| -}
minorMajorSeventh : PitchClass.PitchClass -> Chord
minorMajorSeventh pitchClass =
    Chord.chord pitchClass ChordType.minorMajorSeventh


{-| -}
majorNinth : PitchClass.PitchClass -> Chord
majorNinth pitchClass =
    Chord.chord pitchClass ChordType.majorNinth


{-| -}
minorNinth : PitchClass.PitchClass -> Chord
minorNinth pitchClass =
    Chord.chord pitchClass ChordType.minorNinth


{-| -}
dominantNinth : PitchClass.PitchClass -> Chord
dominantNinth pitchClass =
    Chord.chord pitchClass ChordType.dominantNinth


{-| -}
minorEleventh : PitchClass.PitchClass -> Chord
minorEleventh pitchClass =
    Chord.chord pitchClass ChordType.minorEleventh


{-| -}
dominantEleventh : PitchClass.PitchClass -> Chord
dominantEleventh pitchClass =
    Chord.chord pitchClass ChordType.dominantEleventh


{-| -}
dominantThirteenth : PitchClass.PitchClass -> Chord
dominantThirteenth pitchClass =
    Chord.chord pitchClass ChordType.dominantThirteenth


{-| -}
augmentedDominantSeventh : PitchClass.PitchClass -> Chord
augmentedDominantSeventh pitchClass =
    Chord.chord pitchClass ChordType.augmentedDominantSeventh


{-| -}
dominantSeventhSus4 : PitchClass.PitchClass -> Chord
dominantSeventhSus4 pitchClass =
    Chord.chord pitchClass ChordType.dominantSeventhSus4


{-| -}
dominantSeventhFlatNine : PitchClass.PitchClass -> Chord
dominantSeventhFlatNine pitchClass =
    Chord.chord pitchClass ChordType.dominantSeventhFlatNine


{-| -}
dominantSeventhSharpNine : PitchClass.PitchClass -> Chord
dominantSeventhSharpNine pitchClass =
    Chord.chord pitchClass ChordType.dominantSeventhSharpNine


{-| -}
dominantSeventhFlatNineSharpNine : PitchClass.PitchClass -> Chord
dominantSeventhFlatNineSharpNine pitchClass =
    Chord.chord pitchClass ChordType.dominantSeventhFlatNineSharpNine


{-| -}
dominantSeventhFlatNineSharpEleven : PitchClass.PitchClass -> Chord
dominantSeventhFlatNineSharpEleven pitchClass =
    Chord.chord pitchClass ChordType.dominantSeventhFlatNineSharpEleven


{-| -}
dominantSeventhSharpNineSharpEleven : PitchClass.PitchClass -> Chord
dominantSeventhSharpNineSharpEleven pitchClass =
    Chord.chord pitchClass ChordType.dominantSeventhSharpNineSharpEleven


{-| -}
dominantSeventhSharpEleven : PitchClass.PitchClass -> Chord
dominantSeventhSharpEleven pitchClass =
    Chord.chord pitchClass ChordType.dominantSeventhSharpEleven


{-| -}
dominantSeventhFlatNineFlatThirteen : PitchClass.PitchClass -> Chord
dominantSeventhFlatNineFlatThirteen pitchClass =
    Chord.chord pitchClass ChordType.dominantSeventhFlatNineFlatThirteen


{-| -}
dominantSeventhSharpNineFlatThirteen : PitchClass.PitchClass -> Chord
dominantSeventhSharpNineFlatThirteen pitchClass =
    Chord.chord pitchClass ChordType.dominantSeventhSharpNineFlatThirteen


{-| -}
dominantSeventhSharpElevenFlatThirteen : PitchClass.PitchClass -> Chord
dominantSeventhSharpElevenFlatThirteen pitchClass =
    Chord.chord pitchClass ChordType.dominantSeventhSharpElevenFlatThirteen


{-| Create a chord from a custom chord type. For use with `ChordType.custom`:

    myCustomChordType =
        ChordType.custom
            |> ChordType.withMinorThird
            |> ChordType.withDiminishedFifth
            |> ChordType.withMinorSeventh

    myCustomChord =
        |> custom PitchClass.c myCustomChordType

    -- Equivalent to `halfDiminishedSeventh PitchClass.c`

-}
custom : PitchClass.PitchClass -> ChordType.ChordType -> Chord
custom pitchClass customChordType =
    Chord.chord pitchClass customChordType
