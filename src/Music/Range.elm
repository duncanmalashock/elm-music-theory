module Music.Range exposing
    ( Range
    , range
    , isWithin, min, max
    , sopranoVoice, altoVoice, tenorVoice, bassVoice
    , violin, viola, cello, contrabass
    , bassoon, clarinet, oboe
    , flute, piccolo
    , tuba, frenchHorn, trombone, trumpet
    , altoSax, baritoneSax, tenorSax
    , marimba, vibraphone, glockenspiel, xylophone
    )

{-| A range, like a range of numbers, represents an upper and lower boundary and all the pitches in between. E.g. "C4–C6."

@docs Range


# Constructor

@docs range


# Helpers

@docs isWithin, min, max


# Common instrument ranges

All instrument ranges here are defined in [concert pitch](https://en.wikipedia.org/wiki/Concert_pitch).


## Vocal ranges

@docs sopranoVoice, altoVoice, tenorVoice, bassVoice


## Stringed instruments

@docs violin, viola, cello, contrabass


## Reed instruments

@docs bassoon, clarinet, oboe


## Wind instruments

@docs flute, piccolo


## Brass instruments

@docs tuba, frenchHorn, trombone, trumpet


## Saxophones

@docs altoSax, baritoneSax, tenorSax


## Pitched percussion

@docs marimba, vibraphone, glockenspiel, xylophone

-}

import Music.Internal.InstrumentRanges as InstrumentRanges
import Music.Internal.Pitch as Pitch


{-| -}
type alias Range =
    Pitch.Range


{-| Construct a range:

    range Pitch.c4 Pitch.c6

-}
range : Pitch.Pitch -> Pitch.Pitch -> Range
range lower upper =
    Pitch.range lower upper


{-| Get the lower boundary of a range:

    min sopranoVoice Pitch.c4

-}
min : Range -> Pitch.Pitch
min theRange =
    Pitch.rangeMin theRange


{-| Get the upper boundary of a range:

    max sopranoVoice Pitch.c6

-}
max : Range -> Pitch.Pitch
max theRange =
    Pitch.rangeMax theRange


{-| Find out whether a pitch lies within a range, inclusive of its boundaries.

    isWithin (range Pitch.c4 Pitch.c6) Pitch.c5 == True

-}
isWithin : Range -> Pitch.Pitch -> Bool
isWithin theRange thePitch =
    Pitch.isWithin theRange thePitch



-- Common instrument ranges


{-| -}
sopranoVoice : Pitch.Range
sopranoVoice =
    range Pitch.c4 Pitch.c6


{-| -}
altoVoice : Pitch.Range
altoVoice =
    range Pitch.f3 Pitch.f5


{-| -}
tenorVoice : Pitch.Range
tenorVoice =
    range Pitch.c3 Pitch.c5


{-| -}
bassVoice : Pitch.Range
bassVoice =
    range Pitch.e2 Pitch.e4


{-| -}
violin : Pitch.Range
violin =
    range Pitch.g3 Pitch.a7


{-| -}
viola : Pitch.Range
viola =
    range Pitch.c3 Pitch.e6


{-| -}
cello : Pitch.Range
cello =
    range Pitch.c2 Pitch.c6


{-| -}
contrabass : Pitch.Range
contrabass =
    range Pitch.c1 Pitch.c5


{-| -}
trumpet : Pitch.Range
trumpet =
    range Pitch.e3 Pitch.c6


{-| -}
trombone : Pitch.Range
trombone =
    range Pitch.e2 Pitch.f5


{-| -}
frenchHorn : Pitch.Range
frenchHorn =
    range Pitch.b1 Pitch.f5


{-| -}
tuba : Pitch.Range
tuba =
    range Pitch.d1 Pitch.f4


{-| -}
piccolo : Pitch.Range
piccolo =
    range Pitch.d5 Pitch.b7


{-| -}
flute : Pitch.Range
flute =
    range Pitch.c4 Pitch.d7


{-| -}
oboe : Pitch.Range
oboe =
    range Pitch.b3 Pitch.a6


{-| -}
clarinet : Pitch.Range
clarinet =
    range Pitch.b3 Pitch.a6


{-| -}
altoSax : Pitch.Range
altoSax =
    range Pitch.d3 Pitch.a5


{-| -}
tenorSax : Pitch.Range
tenorSax =
    range Pitch.a2 Pitch.f5


{-| -}
baritoneSax : Pitch.Range
baritoneSax =
    range Pitch.d2 Pitch.a4


{-| -}
bassoon : Pitch.Range
bassoon =
    range Pitch.b1 Pitch.d5


{-| -}
xylophone : Pitch.Range
xylophone =
    range Pitch.g4 Pitch.c7


{-| -}
glockenspiel : Pitch.Range
glockenspiel =
    range Pitch.g3 Pitch.c6


{-| -}
vibraphone : Pitch.Range
vibraphone =
    range Pitch.f3 Pitch.f6


{-| -}
marimba : Pitch.Range
marimba =
    range Pitch.c2 Pitch.c7
