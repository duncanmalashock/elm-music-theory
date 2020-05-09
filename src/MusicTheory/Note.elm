module MusicTheory.Note exposing
    ( Note
    , addDuration
    , dotted
    , duration
    , eighth
    , f
    , ff
    , fff
    , half
    , join
    , louder
    , mf
    , mp
    , multiplyDuration
    , oneHundredTwentyEighth
    , p
    , pitch
    , pp
    , ppp
    , quarter
    , setDuration
    , sixteenth
    , sixtyFourth
    , softer
    , thirtySecond
    , toMidiNoteNumber
    , toMidiVelocity
    , twoHundredFiftySixth
    , whole
    )

import MusicTheory.Dynamics as Dynamics
import MusicTheory.Pitch as Pitch
import MusicTheory.Time as Time


type Note
    = Note Pitch.Pitch Time.Time Dynamics.Dynamics


note : Pitch.Pitch -> Time.Time -> Note
note pit dur =
    Note pit dur Dynamics.normal


pitch : Note -> Pitch.Pitch
pitch (Note pit _ _) =
    pit


toMidiNoteNumber : Note -> Int
toMidiNoteNumber (Note pit dur dyn) =
    Pitch.toMidiNoteNumber pit


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


duration : Note -> Time.Time
duration (Note pit dur dyn) =
    dur


addDuration : Note -> Time.Time -> Note
addDuration (Note pit dur dyn) durationToAdd =
    note pit (Time.add dur durationToAdd)


multiplyDuration : Note -> Time.Time -> Note
multiplyDuration (Note pit dur dyn) durationToMultiply =
    note pit (Time.multiply dur durationToMultiply)


setDuration : Note -> Time.Time -> Note
setDuration (Note pit dur dyn) durationToSet =
    note pit durationToSet


dotted : Note -> Note
dotted (Note pit dur dyn) =
    note pit (Time.add dur (Time.subdivide dur))


join : Note -> Note -> Note
join (Note pitA durationA dynA) (Note pitB durationB dynB) =
    -- This uses only the p of the first Note.
    -- Make sure client code checks that the two pes are the same before joining
    note pitA (Time.add durationA durationB)


whole : Pitch.Pitch -> Note
whole pit =
    note pit Time.whole


half : Pitch.Pitch -> Note
half pit =
    note pit Time.half


quarter : Pitch.Pitch -> Note
quarter pit =
    note pit Time.quarter


eighth : Pitch.Pitch -> Note
eighth pit =
    note pit Time.eighth


sixteenth : Pitch.Pitch -> Note
sixteenth pit =
    note pit Time.sixteenth


thirtySecond : Pitch.Pitch -> Note
thirtySecond pit =
    note pit Time.thirtySecond


sixtyFourth : Pitch.Pitch -> Note
sixtyFourth pit =
    note pit Time.sixtyFourth


oneHundredTwentyEighth : Pitch.Pitch -> Note
oneHundredTwentyEighth pit =
    note pit Time.oneHundredTwentyEighth


twoHundredFiftySixth : Pitch.Pitch -> Note
twoHundredFiftySixth pit =
    note pit Time.twoHundredFiftySixth
