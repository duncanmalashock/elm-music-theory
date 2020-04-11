module MusicTheory.Note exposing
    ( Note
    , addDuration
    , dotted
    , eighth
    , half
    , oneHundredTwentyEighth
    , quarter
    , setDuration
    , sixteenth
    , sixtyFourth
    , thirtySecond
    , twoHundredFiftySixth
    , whole
    )

import MusicTheory.Duration as Duration
import MusicTheory.Pitch as Pitch


type Note
    = Note Pitch.Pitch Duration.Duration


addDuration : Note -> Duration.Duration -> Note
addDuration (Note pitch duration) durationToAdd =
    Note pitch (Duration.add duration durationToAdd)


setDuration : Note -> Duration.Duration -> Note
setDuration (Note pitch duration) durationToSet =
    Note pitch durationToSet


dotted : Note -> Note
dotted (Note pitch duration) =
    Note pitch (Duration.add duration (Duration.subdivide duration))


whole : Pitch.Pitch -> Note
whole pitch =
    Note pitch Duration.whole


half : Pitch.Pitch -> Note
half pitch =
    Note pitch Duration.half


quarter : Pitch.Pitch -> Note
quarter pitch =
    Note pitch Duration.quarter


eighth : Pitch.Pitch -> Note
eighth pitch =
    Note pitch Duration.eighth


sixteenth : Pitch.Pitch -> Note
sixteenth pitch =
    Note pitch Duration.sixteenth


thirtySecond : Pitch.Pitch -> Note
thirtySecond pitch =
    Note pitch Duration.thirtySecond


sixtyFourth : Pitch.Pitch -> Note
sixtyFourth pitch =
    Note pitch Duration.sixtyFourth


oneHundredTwentyEighth : Pitch.Pitch -> Note
oneHundredTwentyEighth pitch =
    Note pitch Duration.oneHundredTwentyEighth


twoHundredFiftySixth : Pitch.Pitch -> Note
twoHundredFiftySixth pitch =
    Note pitch Duration.twoHundredFiftySixth
