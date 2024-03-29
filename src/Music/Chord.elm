module Music.Chord exposing
    ( Chord
    , chordType, root, containsPitchClass, detect
    , toPitchClasses, toString
    , major, minor, augmented, diminished, sus2, sus4
    , majorSix, majorSixNine, minorSix, minorSixNine, majorAddNine, minorAddNine
    , majorSeventh, majorSeventhSharpEleven, minorSeventh, dominantSeventh, diminishedSeventh, halfDiminished, augmentedDominantSeventh, dominantSeventhSus4, minorMajorSeventh
    , majorNinth, minorNinth, dominantNinth, minorEleventh, dominantEleventh, dominantThirteenth
    , dominantSeventhFlatNine, dominantSeventhSharpNine, dominantSeventhFlatNineSharpNine, dominantSeventhFlatNineSharpEleven, dominantSeventhSharpNineSharpEleven, dominantSeventhSharpEleven, dominantSeventhFlatNineFlatThirteen, dominantSeventhSharpNineFlatThirteen, dominantSeventhSharpElevenFlatThirteen, dominantSeventhFlatThirteen
    , dominantNinthSharpEleven, dominantNinthFlatThirteen, dominantNinthSharpElevenFlatThirteen
    , dominantThirteenthFlatNine, dominantThirteenthSharpNine, dominantThirteenthFlatNineSharpNine, dominantThirteenthFlatNineSharpEleven, dominantThirteenthSharpNineSharpEleven, dominantThirteenthSharpEleven
    , custom
    , Serial, toSerial
    )

{-| A [chord](https://en.wikipedia.org/wiki/Chord_%28music%29) is a set of pitch classes that are meant to be sounded together to create [harmony](https://en.wikipedia.org/wiki/Harmony). E.g. a "G dominant seventh" chord.

@docs Chord


# Helpers

@docs chordType, root, containsPitchClass, detect


# Conversion

@docs toPitchClasses, toString


# Voicing chords

A chord is defined by a set of pitch classes. But pitch classes can't be heard; only pitches can be. ["Voicing"](https://en.wikipedia.org/wiki/Voicing_%28music%29) a chord is the process of:

1.  choosing some number of its pitch classes, and
2.  turning them into pitches within specific octaves, so that they can be played or sung.


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


### Seventh

@docs dominantSeventhFlatNine, dominantSeventhSharpNine, dominantSeventhFlatNineSharpNine, dominantSeventhFlatNineSharpEleven, dominantSeventhSharpNineSharpEleven, dominantSeventhSharpEleven, dominantSeventhFlatNineFlatThirteen, dominantSeventhSharpNineFlatThirteen, dominantSeventhSharpElevenFlatThirteen, dominantSeventhFlatThirteen


### Ninth

@docs dominantNinthSharpEleven, dominantNinthFlatThirteen, dominantNinthSharpElevenFlatThirteen


### Thirteenth

@docs dominantThirteenthFlatNine, dominantThirteenthSharpNine, dominantThirteenthFlatNineSharpNine, dominantThirteenthFlatNineSharpEleven, dominantThirteenthSharpNineSharpEleven, dominantThirteenthSharpEleven


# Custom chord types

@docs custom

@docs Serial, toSerial

-}

import Internal.Chord as Chord
import Internal.ChordType as ChordType
import Internal.PitchClass as PitchClass


{-| -}
type alias Chord =
    Chord.Chord


type alias Serial =
    Chord.Serial


toSerial : Chord -> Serial
toSerial chord =
    Chord.toSerial chord


{-| Get a chord's type:

    chordType (dominantSeventh PitchClass.a) == ChordType.dominantSeventh

-}
chordType : Chord -> ChordType.ChordType
chordType theChord =
    Chord.chordType theChord


{-| Get a chord's root:

    root (dominantSeventh PitchClass.a) == PitchClass.a

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


{-| Detect chords that contain the given pitch classes:

    detect
        [ ChordType.majorSix
        , ChordType.minorSeventh
        ]
        [ PitchClass.a
        , PitchClass.c
        , PitchClass.e
        , PitchClass.g
        ]
        == [ minorSeventh PitchClass.a
           , majorSix PitchClass.c
           ]

Passing in a list of chord types to detect is required, since there exists no exhaustive list of valid chords in tonal music.

-}
detect : List ChordType.ChordType -> List PitchClass.PitchClass -> List Chord
detect chordTypesToDetect pitchClasses =
    Chord.detect chordTypesToDetect pitchClasses


{-| Get the pitch classes in a chord:

    toPitchClasses (major PitchClass.a)
        == [ PitchClass.a
           , PitchClass.cSharp
           , PitchClass.e
           ]

Note: for converting a chord to pitches, I recommend looking at the functions in the [Voicing chords](#voicing-chords) section below.

-}
toPitchClasses : Chord -> List PitchClass.PitchClass
toPitchClasses theChord =
    Chord.toPitchClasses theChord


{-| Get the chord symbol for a chord:

    toString (dominantNinth PitchClass.a) == "A9"

Looking for more flexibility? Look at [ChordType.classify](Music-ChordType#classify).

-}
toString : Chord -> String
toString chord =
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
    Chord.chord pitchClass ChordType.halfDiminishedSeventh


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


{-| -}
dominantSeventhFlatThirteen : PitchClass.PitchClass -> Chord
dominantSeventhFlatThirteen pitchClass =
    Chord.chord pitchClass ChordType.dominantSeventhFlatThirteen


{-| -}
dominantNinthSharpEleven : PitchClass.PitchClass -> Chord
dominantNinthSharpEleven pitchClass =
    Chord.chord pitchClass ChordType.dominantNinthSharpEleven


{-| -}
dominantNinthFlatThirteen : PitchClass.PitchClass -> Chord
dominantNinthFlatThirteen pitchClass =
    Chord.chord pitchClass ChordType.dominantNinthFlatThirteen


{-| -}
dominantNinthSharpElevenFlatThirteen : PitchClass.PitchClass -> Chord
dominantNinthSharpElevenFlatThirteen pitchClass =
    Chord.chord pitchClass ChordType.dominantNinthSharpElevenFlatThirteen


{-| -}
dominantThirteenthFlatNine : PitchClass.PitchClass -> Chord
dominantThirteenthFlatNine pitchClass =
    Chord.chord pitchClass ChordType.dominantThirteenthFlatNine


{-| -}
dominantThirteenthSharpNine : PitchClass.PitchClass -> Chord
dominantThirteenthSharpNine pitchClass =
    Chord.chord pitchClass ChordType.dominantThirteenthSharpNine


{-| -}
dominantThirteenthFlatNineSharpNine : PitchClass.PitchClass -> Chord
dominantThirteenthFlatNineSharpNine pitchClass =
    Chord.chord pitchClass ChordType.dominantThirteenthFlatNineSharpNine


{-| -}
dominantThirteenthFlatNineSharpEleven : PitchClass.PitchClass -> Chord
dominantThirteenthFlatNineSharpEleven pitchClass =
    Chord.chord pitchClass ChordType.dominantThirteenthFlatNineSharpEleven


{-| -}
dominantThirteenthSharpNineSharpEleven : PitchClass.PitchClass -> Chord
dominantThirteenthSharpNineSharpEleven pitchClass =
    Chord.chord pitchClass ChordType.dominantThirteenthSharpNineSharpEleven


{-| -}
dominantThirteenthSharpEleven : PitchClass.PitchClass -> Chord
dominantThirteenthSharpEleven pitchClass =
    Chord.chord pitchClass ChordType.dominantThirteenthSharpEleven


{-| Create a chord from a custom chord type. For use with [ChordType.custom](Music-ChordType#custom):

    myCustomChordType =
        ChordType.custom
            |> ChordType.withMinorThird
            |> ChordType.withDiminishedFifth
            |> ChordType.withMinorSeventh

    myCustomChord =
        |> custom PitchClass.c myCustomChordType

    -- Equivalent to `halfDiminishedSeventh PitchClass.c`

You can also use `custom` to construct a chord from a chord type, in cases where the chord type may vary:

    custom PitchClass.c ChordType.majorSeventh
        == Chord.majorSeventh PitchClass.c

-}
custom : PitchClass.PitchClass -> ChordType.ChordType -> Chord
custom pitchClass customChordType =
    Chord.chord pitchClass customChordType
