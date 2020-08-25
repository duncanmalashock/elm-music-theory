module Music.Analysis exposing
    ( Analysis
    , analyze
    , toChord, triadsByDefault, seventhsByDefault
    , symbol
    , i, ii, iii, iv, v, vi, vii
    , withChordType
    , iSharp, iiSharp, iiiSharp, ivSharp, vSharp, viSharp, viiSharp
    , iFlat, iiFlat, iiiFlat, ivFlat, vFlat, viFlat, viiFlat
    )

{-| [Roman numeral analysis](https://en.wikipedia.org/wiki/Roman_numeral_analysis) describes the relationship of a chord to a key.

@docs Analysis


# Analysis

@docs analyze


# Converting to chords

@docs toChord, triadsByDefault, seventhsByDefault


# Other conversions

@docs symbol


# Constructors


## Diatonic scale degrees

@docs i, ii, iii, iv, v, vi, vii

@docs withChordType


## Chromatic scale degrees

Chromatic scale degrees require a chord type to be specified, since they have no defaults:

@docs iSharp, iiSharp, iiiSharp, ivSharp, vSharp, viSharp, viiSharp
@docs iFlat, iiFlat, iiiFlat, ivFlat, vFlat, viFlat, viiFlat


## Secondary dominants

TK

-}

import Music.Chord as Chord
import Music.ChordType as ChordType
import Music.Internal.Analysis as Analysis
import Music.Internal.Key as Key


{-| -}
type alias Analysis =
    Analysis.Analysis


{-| Analyze a chord in the context of a given key:

    analyze (Chord.minor PitchClass.e) Key.c == iii

-}
analyze : Chord.Chord -> Key.Key -> Analysis
analyze chord key =
    Analysis.fromChord key chord


{-| Convert to a chord in the specified key:

    toChord triadsByDefault Key.f iii == Chord.minor PitchClass.a

-}
toChord : Analysis.DefaultChordTypes -> Key.Key -> Analysis -> Chord.Chord
toChord defaults key analysis =
    toChord defaults key analysis


{-| Use triads when converting, if no chord type is specified:

    toChord triadsByDefault Key.f iv == Chord.major PitchClass.bFlat

-}
triadsByDefault : Analysis.DefaultChordTypes
triadsByDefault =
    Analysis.triadsByDefault


{-| Use seventh chords when converting, if no chord type is specified:

    toChord triadsByDefault Key.f iv == Chord.majorSeventh PitchClass.bFlat

-}
seventhsByDefault : Analysis.DefaultChordTypes
seventhsByDefault =
    Analysis.seventhsByDefault


{-| Get the symbol for a Roman numeral analysis:

    symbol Key.c iv == "IV"

    symbol Key.cMinor iv == "iv"

-}
symbol : Key.Key -> Analysis -> String
symbol key analysis =
    Analysis.symbol key analysis


{-| -}
i : Analysis
i =
    Analysis.i


{-| -}
iSharp : ChordType.ChordType -> Analysis
iSharp =
    Analysis.iSharp


{-| -}
iFlat : ChordType.ChordType -> Analysis
iFlat =
    Analysis.iFlat


{-| -}
ii : Analysis
ii =
    Analysis.ii


{-| -}
iiSharp : ChordType.ChordType -> Analysis
iiSharp =
    Analysis.iiSharp


{-| -}
iiFlat : ChordType.ChordType -> Analysis
iiFlat =
    Analysis.iiFlat


{-| -}
iii : Analysis
iii =
    Analysis.iii


{-| -}
iiiSharp : ChordType.ChordType -> Analysis
iiiSharp =
    Analysis.iiiSharp


{-| -}
iiiFlat : ChordType.ChordType -> Analysis
iiiFlat =
    Analysis.iiiFlat


{-| -}
iv : Analysis
iv =
    Analysis.iv


{-| -}
ivSharp : ChordType.ChordType -> Analysis
ivSharp =
    Analysis.ivSharp


{-| -}
ivFlat : ChordType.ChordType -> Analysis
ivFlat =
    Analysis.ivFlat


{-| -}
v : Analysis
v =
    Analysis.v


{-| -}
vSharp : ChordType.ChordType -> Analysis
vSharp =
    Analysis.vSharp


{-| -}
vFlat : ChordType.ChordType -> Analysis
vFlat =
    Analysis.vFlat


{-| -}
vi : Analysis
vi =
    Analysis.vi


{-| -}
viSharp : ChordType.ChordType -> Analysis
viSharp =
    Analysis.viSharp


{-| -}
viFlat : ChordType.ChordType -> Analysis
viFlat =
    Analysis.viFlat


{-| -}
vii : Analysis
vii =
    Analysis.vii


{-| -}
viiFlat : ChordType.ChordType -> Analysis
viiFlat =
    Analysis.viiFlat


{-| -}
viiSharp : ChordType.ChordType -> Analysis
viiSharp =
    Analysis.viiSharp


{-| Diatonic scale degrees have default chords types in major or minor scales. This function lets you use others:

    iii
        |> withChordType ChordType.majorSeventh
        |> symbol -- "IIImaj7"

-}
withChordType : ChordType.ChordType -> Analysis -> Analysis
withChordType chordType analysis =
    withChordType chordType analysis
