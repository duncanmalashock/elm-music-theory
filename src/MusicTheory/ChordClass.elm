module MusicTheory.ChordClass exposing
    ( ChordClass(..)
    , ChordClassError(..)
    , all
    , augmented
    , diminished
    , diminishedSeventh
    , diminishedSeventhElevenFlatThirteen
    , dominantEleventh
    , dominantNinth
    , dominantSeventh
    , dominantSeventhFlatNine
    , dominantSeventhFlatNineFlatThirteen
    , dominantSeventhFlatNineSharpNine
    , dominantSeventhFlatNineSharpNineFlatThirteen
    , dominantSeventhSharpNine
    , dominantSeventhSharpNineFlatThirteen
    , dominantSeventhSus4
    , dominantThirteenth
    , dominantThirteenthFlatNine
    , dominantThirteenthSharpNine
    , dominantThirteenthSharpNineFlatNine
    , errorToString
    , halfDiminished
    , major
    , majorAddNine
    , majorSeventh
    , majorSeventhSharpEleven
    , majorSix
    , majorSixNine
    , minor
    , minorAddNine
    , minorMajorSeventh
    , minorNinth
    , minorSeventh
    , minorSix
    , minorSixNine
    , nonTertian
    , sus2
    , sus4
    , toIntervals
    , toTertianFactors
    )

import MusicTheory.Interval exposing (Interval)
import MusicTheory.TertianFactors as TertianFactors
    exposing
        ( Alteration(..)
        , Extension(..)
        , TertianFactors
        )


type ChordClass
    = Tertian TertianFactors
    | NonTertian (List Interval)


toIntervals : ChordClass -> List Interval
toIntervals chordClass =
    case chordClass of
        Tertian tertianFactors ->
            TertianFactors.toIntervals tertianFactors

        NonTertian intervals ->
            intervals


toTertianFactors : ChordClass -> Result ChordClassError TertianFactors.TertianFactors
toTertianFactors chordClass =
    case chordClass of
        NonTertian _ ->
            Err (ChordClassIsNonTertian chordClass)

        Tertian tertianFactors ->
            Ok tertianFactors


type ChordClassError
    = ChordClassIsNonTertian ChordClass


errorToString : ChordClassError -> String
errorToString error =
    case error of
        ChordClassIsNonTertian chordClass ->
            "Cannot convert non-tertian chord class to tertian chord factors."


all : List ChordClass
all =
    [ augmented
    , diminished
    , diminishedSeventh
    , diminishedSeventhElevenFlatThirteen
    , dominantEleventh
    , dominantNinth
    , dominantSeventh
    , dominantSeventhFlatNine
    , dominantSeventhFlatNineFlatThirteen
    , dominantSeventhFlatNineSharpNine
    , dominantSeventhFlatNineSharpNineFlatThirteen
    , dominantSeventhSharpNine
    , dominantSeventhSharpNineFlatThirteen
    , dominantSeventhSus4
    , dominantThirteenth
    , dominantThirteenthFlatNine
    , dominantThirteenthSharpNine
    , dominantThirteenthSharpNineFlatNine
    , halfDiminished
    , major
    , majorAddNine
    , majorSeventh
    , majorSix
    , majorSixNine
    , minor
    , minorAddNine
    , minorMajorSeventh
    , minorNinth
    , minorSeventh
    , minorSix
    , minorSixNine
    , sus2
    , sus4
    , majorSeventhSharpEleven
    ]



-- Triads


major : ChordClass
major =
    TertianFactors.tertianFactors
        |> TertianFactors.withMajorThird
        |> TertianFactors.withFifth
        |> Tertian


minor : ChordClass
minor =
    TertianFactors.tertianFactors
        |> TertianFactors.withMinorThird
        |> TertianFactors.withFifth
        |> Tertian


augmented : ChordClass
augmented =
    TertianFactors.tertianFactors
        |> TertianFactors.withMajorThird
        |> TertianFactors.withSharpFifth
        |> Tertian


diminished : ChordClass
diminished =
    TertianFactors.tertianFactors
        |> TertianFactors.withMinorThird
        |> TertianFactors.withFlatFifth
        |> Tertian


sus2 : ChordClass
sus2 =
    TertianFactors.tertianFactors
        |> TertianFactors.withSuspendedSecond
        |> TertianFactors.withFifth
        |> Tertian


sus4 : ChordClass
sus4 =
    TertianFactors.tertianFactors
        |> TertianFactors.withSuspendedFourth
        |> TertianFactors.withFifth
        |> Tertian



-- Add9 Chords


majorAddNine : ChordClass
majorAddNine =
    TertianFactors.tertianFactors
        |> TertianFactors.withMajorThird
        |> TertianFactors.withFifth
        |> TertianFactors.withNinth
        |> Tertian


minorAddNine : ChordClass
minorAddNine =
    TertianFactors.tertianFactors
        |> TertianFactors.withMinorThird
        |> TertianFactors.withFifth
        |> TertianFactors.withNinth
        |> Tertian



-- Sixth Chords


majorSix : ChordClass
majorSix =
    TertianFactors.tertianFactors
        |> TertianFactors.withMajorThird
        |> TertianFactors.withFifth
        |> TertianFactors.withSixth
        |> Tertian


minorSix : ChordClass
minorSix =
    TertianFactors.tertianFactors
        |> TertianFactors.withMinorThird
        |> TertianFactors.withFifth
        |> TertianFactors.withSixth
        |> Tertian


majorSixNine : ChordClass
majorSixNine =
    TertianFactors.tertianFactors
        |> TertianFactors.withMajorThird
        |> TertianFactors.withFifth
        |> TertianFactors.withSixth
        |> TertianFactors.withNinth
        |> Tertian


minorSixNine : ChordClass
minorSixNine =
    TertianFactors.tertianFactors
        |> TertianFactors.withMinorThird
        |> TertianFactors.withFifth
        |> TertianFactors.withSixth
        |> TertianFactors.withNinth
        |> Tertian



-- Seventh Chords


majorSeventh : ChordClass
majorSeventh =
    TertianFactors.tertianFactors
        |> TertianFactors.withMajorThird
        |> TertianFactors.withFifth
        |> TertianFactors.withMajorSeventh
        |> Tertian


majorSeventhSharpEleven : ChordClass
majorSeventhSharpEleven =
    TertianFactors.tertianFactors
        |> TertianFactors.withMajorThird
        |> TertianFactors.withFifth
        |> TertianFactors.withMajorSeventh
        |> TertianFactors.withSharpEleventh
        |> Tertian


minorSeventh : ChordClass
minorSeventh =
    TertianFactors.tertianFactors
        |> TertianFactors.withMinorThird
        |> TertianFactors.withFifth
        |> TertianFactors.withMinorSeventh
        |> Tertian


minorNinth : ChordClass
minorNinth =
    TertianFactors.tertianFactors
        |> TertianFactors.withMinorThird
        |> TertianFactors.withFifth
        |> TertianFactors.withMinorSeventh
        |> TertianFactors.withNinth
        |> Tertian


dominantSeventh : ChordClass
dominantSeventh =
    TertianFactors.tertianFactors
        |> TertianFactors.withMajorThird
        |> TertianFactors.withFifth
        |> TertianFactors.withMinorSeventh
        |> Tertian


dominantSeventhSus4 : ChordClass
dominantSeventhSus4 =
    TertianFactors.tertianFactors
        |> TertianFactors.withSuspendedFourth
        |> TertianFactors.withFifth
        |> TertianFactors.withMinorSeventh
        |> Tertian


minorMajorSeventh : ChordClass
minorMajorSeventh =
    TertianFactors.tertianFactors
        |> TertianFactors.withMinorThird
        |> TertianFactors.withFifth
        |> TertianFactors.withMajorSeventh
        |> Tertian


halfDiminished : ChordClass
halfDiminished =
    TertianFactors.tertianFactors
        |> TertianFactors.withMinorThird
        |> TertianFactors.withFlatFifth
        |> TertianFactors.withMinorSeventh
        |> Tertian


diminishedSeventh : ChordClass
diminishedSeventh =
    TertianFactors.tertianFactors
        |> TertianFactors.withMinorThird
        |> TertianFactors.withFlatFifth
        |> TertianFactors.withDiminishedSeventh
        |> Tertian


diminishedSeventhElevenFlatThirteen : ChordClass
diminishedSeventhElevenFlatThirteen =
    TertianFactors.tertianFactors
        |> TertianFactors.withMinorThird
        |> TertianFactors.withFlatFifth
        |> TertianFactors.withDiminishedSeventh
        |> TertianFactors.withFlatThirteenth
        |> Tertian



-- Extended Dominants, no altered tones


dominantNinth : ChordClass
dominantNinth =
    TertianFactors.tertianFactors
        |> TertianFactors.withMajorThird
        |> TertianFactors.withFifth
        |> TertianFactors.withMinorSeventh
        |> TertianFactors.withNinth
        |> Tertian


dominantEleventh : ChordClass
dominantEleventh =
    TertianFactors.tertianFactors
        |> TertianFactors.withMajorThird
        |> TertianFactors.withFifth
        |> TertianFactors.withMinorSeventh
        |> TertianFactors.withNinth
        |> TertianFactors.withEleventh
        |> Tertian


dominantThirteenth : ChordClass
dominantThirteenth =
    TertianFactors.tertianFactors
        |> TertianFactors.withMajorThird
        |> TertianFactors.withFifth
        |> TertianFactors.withMinorSeventh
        |> TertianFactors.withNinth
        |> TertianFactors.withEleventh
        |> TertianFactors.withThirteenth
        |> Tertian



-- -- Dominant Seventh Chords, Altered


dominantSeventhSharpNine : ChordClass
dominantSeventhSharpNine =
    TertianFactors.tertianFactors
        |> TertianFactors.withMajorThird
        |> TertianFactors.withFifth
        |> TertianFactors.withMinorSeventh
        |> TertianFactors.withSharpNinth
        |> Tertian


dominantSeventhFlatNine : ChordClass
dominantSeventhFlatNine =
    TertianFactors.tertianFactors
        |> TertianFactors.withMajorThird
        |> TertianFactors.withFifth
        |> TertianFactors.withMinorSeventh
        |> TertianFactors.withFlatNinth
        |> Tertian


dominantSeventhFlatNineSharpNine : ChordClass
dominantSeventhFlatNineSharpNine =
    TertianFactors.tertianFactors
        |> TertianFactors.withMajorThird
        |> TertianFactors.withFifth
        |> TertianFactors.withMinorSeventh
        |> TertianFactors.withFlatNinth
        |> TertianFactors.withSharpNinth
        |> Tertian


dominantSeventhSharpNineFlatThirteen : ChordClass
dominantSeventhSharpNineFlatThirteen =
    TertianFactors.tertianFactors
        |> TertianFactors.withMajorThird
        |> TertianFactors.withFifth
        |> TertianFactors.withMinorSeventh
        |> TertianFactors.withSharpNinth
        |> TertianFactors.withFlatThirteenth
        |> Tertian


dominantSeventhFlatNineFlatThirteen : ChordClass
dominantSeventhFlatNineFlatThirteen =
    TertianFactors.tertianFactors
        |> TertianFactors.withMajorThird
        |> TertianFactors.withFifth
        |> TertianFactors.withMinorSeventh
        |> TertianFactors.withFlatNinth
        |> TertianFactors.withFlatThirteenth
        |> Tertian


dominantSeventhFlatNineSharpNineFlatThirteen : ChordClass
dominantSeventhFlatNineSharpNineFlatThirteen =
    TertianFactors.tertianFactors
        |> TertianFactors.withMajorThird
        |> TertianFactors.withFifth
        |> TertianFactors.withMinorSeventh
        |> TertianFactors.withFlatNinth
        |> TertianFactors.withSharpNinth
        |> TertianFactors.withFlatThirteenth
        |> Tertian


dominantThirteenthFlatNine : ChordClass
dominantThirteenthFlatNine =
    TertianFactors.tertianFactors
        |> TertianFactors.withMajorThird
        |> TertianFactors.withFifth
        |> TertianFactors.withMinorSeventh
        |> TertianFactors.withFlatNinth
        |> TertianFactors.withThirteenth
        |> Tertian


dominantThirteenthSharpNine : ChordClass
dominantThirteenthSharpNine =
    TertianFactors.tertianFactors
        |> TertianFactors.withMajorThird
        |> TertianFactors.withFifth
        |> TertianFactors.withMinorSeventh
        |> TertianFactors.withSharpNinth
        |> TertianFactors.withThirteenth
        |> Tertian


dominantThirteenthSharpNineFlatNine : ChordClass
dominantThirteenthSharpNineFlatNine =
    TertianFactors.tertianFactors
        |> TertianFactors.withMajorThird
        |> TertianFactors.withFifth
        |> TertianFactors.withMinorSeventh
        |> TertianFactors.withFlatNinth
        |> TertianFactors.withSharpNinth
        |> TertianFactors.withThirteenth
        |> Tertian


nonTertian : List Interval -> ChordClass
nonTertian intervals =
    NonTertian intervals
