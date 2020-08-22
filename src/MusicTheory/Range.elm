module MusicTheory.Range exposing
    ( Range
    , range
    , isWithin
    , sopranoVoice, altoVoice, tenorVoice, bassVoice
    , violin, viola, cello
    )

{-| A range, like a range of numbers, represents an upper and lower boundary and all the pitches in between.

@docs Range
@docs range


# Helpers

@docs isWithin


# Common instrument ranges

@docs sopranoVoice, altoVoice, tenorVoice, bassVoice
@docs violin, viola, cello

-}

import MusicTheory.Internal.InstrumentRanges as InstrumentRanges
import MusicTheory.Internal.Pitch as Pitch


{-| -}
type alias Range =
    Pitch.Range


{-| Construct a range:

    range Pitch.c4 Pitch.c6

-}
range : Pitch.Pitch -> Pitch.Pitch -> Range
range lower upper =
    Pitch.range lower upper


{-| Find out whether a pitch lies within a range, inclusive of its boundaries.

    isWithin (range Pitch.c4 Pitch.c6) Pitch.c5 == True

-}
isWithin : Range -> Pitch.Pitch -> Bool
isWithin theRange thePitch =
    Pitch.isWithin theRange thePitch



-- Common instrument ranges


{-| -}
sopranoVoice : Range
sopranoVoice =
    InstrumentRanges.sopranoVoice


{-| -}
altoVoice : Range
altoVoice =
    InstrumentRanges.altoVoice


{-| -}
tenorVoice : Range
tenorVoice =
    InstrumentRanges.tenorVoice


{-| -}
bassVoice : Range
bassVoice =
    InstrumentRanges.bassVoice


{-| -}
violin : Range
violin =
    InstrumentRanges.violin


{-| -}
viola : Range
viola =
    InstrumentRanges.viola


{-| -}
cello : Range
cello =
    InstrumentRanges.cello
