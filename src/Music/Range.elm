module Music.Range exposing
    ( Range, range
    , isWithin, min, max
    , piano
    , sopranoVoice, mezzoSopranoVoice, altoVoice
    , countertenorVoice, tenorVoice, bassVoice
    , violin, viola, cello, contrabass
    , flute, piccolo
    , oboe, englishHorn
    , clarinet, bassClarinet
    , bassoon, contrabassoon
    , trumpet, frenchHorn, trombone, tuba
    , sopranoSax, altoSax, tenorSax, baritoneSax
    , marimba, vibraphone, glockenspiel, xylophone
    )

{-| A range, like a range of numbers, represents an upper and lower boundary and all the pitches in between. E.g. "C4–C6."

@docs Range, range


# Helpers

@docs isWithin, min, max


# Common instrument ranges

All instrument ranges here are defined in [concert pitch](https://en.wikipedia.org/wiki/Concert_pitch).


## Piano

@docs piano


## Vocal ranges


### Female vocal ranges

@docs sopranoVoice, mezzoSopranoVoice, altoVoice


### Male vocal ranges

@docs countertenorVoice, tenorVoice, bassVoice


## String family

@docs violin, viola, cello, contrabass


## Wind instruments

@docs flute, piccolo


## Reed instruments

@docs oboe, englishHorn
@docs clarinet, bassClarinet
@docs bassoon, contrabassoon


## Brass instruments

@docs trumpet, frenchHorn, trombone, tuba


## Saxophones

@docs sopranoSax, altoSax, tenorSax, baritoneSax


## Pitched percussion

@docs marimba, vibraphone, glockenspiel, xylophone

-}

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

    min sopranoVoice == Pitch.c4

-}
min : Range -> Pitch.Pitch
min theRange =
    Pitch.rangeMin theRange


{-| Get the upper boundary of a range:

    max sopranoVoice == Pitch.c6

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
piano : Pitch.Range
piano =
    range Pitch.a0 Pitch.c8


{-| -}
sopranoVoice : Pitch.Range
sopranoVoice =
    range Pitch.c4 Pitch.c6


{-| -}
mezzoSopranoVoice : Pitch.Range
mezzoSopranoVoice =
    range Pitch.a3 Pitch.a5


{-| -}
altoVoice : Pitch.Range
altoVoice =
    range Pitch.f3 Pitch.f5


{-| -}
countertenorVoice : Pitch.Range
countertenorVoice =
    range Pitch.g3 Pitch.e5


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
englishHorn : Pitch.Range
englishHorn =
    range Pitch.e3 Pitch.c6


{-| -}
clarinet : Pitch.Range
clarinet =
    range Pitch.b3 Pitch.a6


{-| -}
bassClarinet : Pitch.Range
bassClarinet =
    range Pitch.bFlat1 Pitch.b5


{-| -}
contrabassoon : Pitch.Range
contrabassoon =
    range Pitch.bFlat0 Pitch.d4


{-| -}
sopranoSax : Pitch.Range
sopranoSax =
    range Pitch.aFlat3 Pitch.e6


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
