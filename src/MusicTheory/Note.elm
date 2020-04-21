module MusicTheory.Note exposing
    ( Note
    , addDuration
    , dotted
    , duration
    , eighth
    , half
    , join
    , oneHundredTwentyEighth
    , quarter
    , setDuration
    , sixteenth
    , sixtyFourth
    , thirtySecond
    , toMidiNote
    , twoHundredFiftySixth
    , whole
    )

import MusicTheory.Pitch as Pitch
import MusicTheory.Time as Time


type Note
    = Note Pitch.Pitch Time.Time


toMidiNote : Note -> Int
toMidiNote (Note pitch dur) =
    Pitch.toMidiNote pitch


duration : Note -> Time.Time
duration (Note pitch dur) =
    dur


addDuration : Note -> Time.Time -> Note
addDuration (Note pitch dur) durationToAdd =
    Note pitch (Time.add dur durationToAdd)


setDuration : Note -> Time.Time -> Note
setDuration (Note pitch dur) durationToSet =
    Note pitch durationToSet


dotted : Note -> Note
dotted (Note pitch dur) =
    Note pitch (Time.add dur (Time.subdivide dur))


join : Note -> Note -> Note
join (Note pitchA durationA) (Note pitchB durationB) =
    -- This uses only the pitch of the first Note.
    -- Make sure client code checks that the two pitches are the same before joining
    Note pitchA (Time.add durationA durationB)


whole : Pitch.Pitch -> Note
whole pitch =
    Note pitch Time.whole


half : Pitch.Pitch -> Note
half pitch =
    Note pitch Time.half


quarter : Pitch.Pitch -> Note
quarter pitch =
    Note pitch Time.quarter


eighth : Pitch.Pitch -> Note
eighth pitch =
    Note pitch Time.eighth


sixteenth : Pitch.Pitch -> Note
sixteenth pitch =
    Note pitch Time.sixteenth


thirtySecond : Pitch.Pitch -> Note
thirtySecond pitch =
    Note pitch Time.thirtySecond


sixtyFourth : Pitch.Pitch -> Note
sixtyFourth pitch =
    Note pitch Time.sixtyFourth


oneHundredTwentyEighth : Pitch.Pitch -> Note
oneHundredTwentyEighth pitch =
    Note pitch Time.oneHundredTwentyEighth


twoHundredFiftySixth : Pitch.Pitch -> Note
twoHundredFiftySixth pitch =
    Note pitch Time.twoHundredFiftySixth
