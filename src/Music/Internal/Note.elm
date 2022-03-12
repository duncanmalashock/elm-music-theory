module Music.Internal.Note exposing
    ( Note, note
    , whole, half, quarter, eighth, sixteenth
    , thirtySecond, sixtyFourth, oneHundredTwentyEighth, twoHundredFiftySixth
    , dotted
    , mf, f, ff, fff
    , mp, p, pp, ppp
    , louder, softer
    , pitch, duration
    , addDuration, setDuration, multiplyDuration
    , join
    , toMidiNoteNumber, toMidiVelocity
    )

{-|

@docs Note, note

@docs whole, half, quarter, eighth, sixteenth
@docs thirtySecond, sixtyFourth, oneHundredTwentyEighth, twoHundredFiftySixth

@docs dotted

@docs mf, f, ff, fff
@docs mp, p, pp, ppp

@docs louder, softer

@docs pitch, duration

@docs addDuration, setDuration, multiplyDuration

@docs join

@docs toMidiNoteNumber, toMidiVelocity

-}

import Music.Internal.Duration as Duration
import Music.Internal.Dynamics as Dynamics
import Music.Internal.Pitch as Pitch


type Note
    = Note Pitch.Pitch Duration.Duration Dynamics.Dynamics


note : Pitch.Pitch -> Duration.Duration -> Note
note pit dur =
    Note pit dur Dynamics.normal


pitch : Note -> Pitch.Pitch
pitch (Note pit _ _) =
    pit


toMidiNoteNumber : Note -> Int
toMidiNoteNumber (Note pit dur dyn) =
    Pitch.toMIDINoteNumber pit


toMidiVelocity : Note -> Int
toMidiVelocity (Note pit dur dyn) =
    Dynamics.toMidiVelocity dyn


mf : Note -> Note
mf (Note pit dur dyn) =
    Note pit dur Dynamics.mf


f : Note -> Note
f (Note pit dur dyn) =
    Note pit dur Dynamics.f


ff : Note -> Note
ff (Note pit dur dyn) =
    Note pit dur Dynamics.ff


fff : Note -> Note
fff (Note pit dur dyn) =
    Note pit dur Dynamics.fff


mp : Note -> Note
mp (Note pit dur dyn) =
    Note pit dur Dynamics.mp


p : Note -> Note
p (Note pit dur dyn) =
    Note pit dur Dynamics.p


pp : Note -> Note
pp (Note pit dur dyn) =
    Note pit dur Dynamics.pp


ppp : Note -> Note
ppp (Note pit dur dyn) =
    Note pit dur Dynamics.ppp


softer : Note -> Note
softer (Note pit dur dyn) =
    Note pit dur (Dynamics.softer dyn)


louder : Note -> Note
louder (Note pit dur dyn) =
    Note pit dur (Dynamics.louder dyn)


duration : Note -> Duration.Duration
duration (Note pit dur dyn) =
    dur


addDuration : Duration.Duration -> Note -> Note
addDuration durationToAdd (Note pit dur dyn) =
    note pit (Duration.add dur durationToAdd)


multiplyDuration : Note -> Duration.Duration -> Note
multiplyDuration (Note pit dur dyn) durationToMultiply =
    note pit (Duration.multiply dur durationToMultiply)


setDuration : Note -> Duration.Duration -> Note
setDuration (Note pit dur dyn) durationToSet =
    note pit durationToSet


dotted : Note -> Note
dotted (Note pit dur dyn) =
    note pit (Duration.add dur (Duration.subdivide dur))


join : Note -> Note -> Note
join (Note pitA durationA dynA) (Note pitB durationB dynB) =
    -- This uses only the p of the first Note.
    -- Make sure client code checks that the two pes are the same before joining
    note pitA (Duration.add durationA durationB)


whole : Pitch.Pitch -> Note
whole pit =
    note pit Duration.whole


half : Pitch.Pitch -> Note
half pit =
    note pit Duration.half


quarter : Pitch.Pitch -> Note
quarter pit =
    note pit Duration.quarter


eighth : Pitch.Pitch -> Note
eighth pit =
    note pit Duration.eighth


sixteenth : Pitch.Pitch -> Note
sixteenth pit =
    note pit Duration.sixteenth


thirtySecond : Pitch.Pitch -> Note
thirtySecond pit =
    note pit Duration.thirtySecond


sixtyFourth : Pitch.Pitch -> Note
sixtyFourth pit =
    note pit Duration.sixtyFourth


oneHundredTwentyEighth : Pitch.Pitch -> Note
oneHundredTwentyEighth pit =
    note pit Duration.oneHundredTwentyEighth


twoHundredFiftySixth : Pitch.Pitch -> Note
twoHundredFiftySixth pit =
    note pit Duration.twoHundredFiftySixth
