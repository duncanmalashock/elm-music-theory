module MusicTheory.Note exposing
    ( Note
    , addDuration
    , dotted
    , duration
    , eighth
    , half
    , join
    , louder
    , multiplyDuration
    , oneHundredTwentyEighth
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
note pitch dur =
    Note pitch dur Dynamics.normal


toMidiNoteNumber : Note -> Int
toMidiNoteNumber (Note pitch dur dyn) =
    Pitch.toMidiNoteNumber pitch


toMidiVelocity : Note -> Int
toMidiVelocity (Note pitch dur dyn) =
    Dynamics.toMidiVelocity dyn


softer : Note -> Note
softer (Note pitch dur dyn) =
    Note pitch dur (Dynamics.softer dyn)


louder : Note -> Note
louder (Note pitch dur dyn) =
    Note pitch dur (Dynamics.louder dyn)


duration : Note -> Time.Time
duration (Note pitch dur dyn) =
    dur


addDuration : Note -> Time.Time -> Note
addDuration (Note pitch dur dyn) durationToAdd =
    note pitch (Time.add dur durationToAdd)


multiplyDuration : Note -> Time.Time -> Note
multiplyDuration (Note pitch dur dyn) durationToMultiply =
    note pitch (Time.multiply dur durationToMultiply)


setDuration : Note -> Time.Time -> Note
setDuration (Note pitch dur dyn) durationToSet =
    note pitch durationToSet


dotted : Note -> Note
dotted (Note pitch dur dyn) =
    note pitch (Time.add dur (Time.subdivide dur))


join : Note -> Note -> Note
join (Note pitchA durationA dynA) (Note pitchB durationB dynB) =
    -- This uses only the pitch of the first Note.
    -- Make sure client code checks that the two pitches are the same before joining
    note pitchA (Time.add durationA durationB)


whole : Pitch.Pitch -> Note
whole pitch =
    note pitch Time.whole


half : Pitch.Pitch -> Note
half pitch =
    note pitch Time.half


quarter : Pitch.Pitch -> Note
quarter pitch =
    note pitch Time.quarter


eighth : Pitch.Pitch -> Note
eighth pitch =
    note pitch Time.eighth


sixteenth : Pitch.Pitch -> Note
sixteenth pitch =
    note pitch Time.sixteenth


thirtySecond : Pitch.Pitch -> Note
thirtySecond pitch =
    note pitch Time.thirtySecond


sixtyFourth : Pitch.Pitch -> Note
sixtyFourth pitch =
    note pitch Time.sixtyFourth


oneHundredTwentyEighth : Pitch.Pitch -> Note
oneHundredTwentyEighth pitch =
    note pitch Time.oneHundredTwentyEighth


twoHundredFiftySixth : Pitch.Pitch -> Note
twoHundredFiftySixth pitch =
    note pitch Time.twoHundredFiftySixth
