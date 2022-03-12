module Music.Tempo exposing
    ( Tempo
    , quarterNotesPerMinute
    , larghissimo
    , adagissimo
    , grave
    , largo
    , lento
    , larghetto
    , adagio
    , adagietto
    , andante
    , moderato
    , allegretto
    , allegro
    , vivace
    , vivacissimo
    , presto
    , prestissimo
    )

{-|

@docs Tempo

@docs quarterNotesPerMinute

@docs larghissimo
@docs adagissimo
@docs grave
@docs largo
@docs lento
@docs larghetto
@docs adagio
@docs adagietto
@docs andante
@docs moderato
@docs allegretto
@docs allegro
@docs vivace
@docs vivacissimo
@docs presto
@docs prestissimo

-}

import Music.Duration as Duration exposing (Duration)


type Tempo
    = Tempo
        { beatsPerMinute : Int
        , beatUnit : Duration
        }


quarterNotesPerMinute : Int -> Tempo
quarterNotesPerMinute bpm =
    custom bpm Duration.quarter


larghissimo : Tempo
larghissimo =
    quarterNotesPerMinute 24


adagissimo : Tempo
adagissimo =
    quarterNotesPerMinute 40


grave : Tempo
grave =
    quarterNotesPerMinute 35


largo : Tempo
largo =
    quarterNotesPerMinute 50


lento : Tempo
lento =
    quarterNotesPerMinute 60


larghetto : Tempo
larghetto =
    quarterNotesPerMinute 65


adagio : Tempo
adagio =
    quarterNotesPerMinute 70


adagietto : Tempo
adagietto =
    quarterNotesPerMinute 74


andante : Tempo
andante =
    quarterNotesPerMinute 100


moderato : Tempo
moderato =
    quarterNotesPerMinute 112


allegretto : Tempo
allegretto =
    quarterNotesPerMinute 118


allegro : Tempo
allegro =
    quarterNotesPerMinute 130


vivace : Tempo
vivace =
    quarterNotesPerMinute 165


vivacissimo : Tempo
vivacissimo =
    quarterNotesPerMinute 174


presto : Tempo
presto =
    quarterNotesPerMinute 185


prestissimo : Tempo
prestissimo =
    quarterNotesPerMinute 200


custom : Int -> Duration -> Tempo
custom bpm unit =
    Tempo
        { beatsPerMinute = bpm
        , beatUnit = unit
        }
