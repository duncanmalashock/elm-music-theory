module Music.Dynamics exposing
    ( Dynamics
    , normal
    , mf, f, ff, fff, ffff
    , mp, p, pp, ppp, pppp
    , louder, softer
    , toMidiVelocity
    , Serial, toSerial
    )

{-|

@docs Dynamics

@docs normal
@docs mf, f, ff, fff, ffff
@docs mp, p, pp, ppp, pppp

@docs louder, softer

@docs toMidiVelocity

@docs Serial, toSerial

-}


type Dynamics
    = Dynamics Int


type alias Serial =
    Int


toSerial : Dynamics -> Serial
toSerial (Dynamics d) =
    d


normal : Dynamics
normal =
    Dynamics 64


mf : Dynamics
mf =
    Dynamics 75


f : Dynamics
f =
    Dynamics 88


ff : Dynamics
ff =
    Dynamics 101


fff : Dynamics
fff =
    Dynamics 114


ffff : Dynamics
ffff =
    Dynamics 127


mp : Dynamics
mp =
    Dynamics 62


p : Dynamics
p =
    Dynamics 49


pp : Dynamics
pp =
    Dynamics 36


ppp : Dynamics
ppp =
    Dynamics 23


pppp : Dynamics
pppp =
    Dynamics 10


louder : Dynamics -> Dynamics
louder (Dynamics level) =
    Dynamics (level + 10)


softer : Dynamics -> Dynamics
softer (Dynamics level) =
    Dynamics (level - 10)


toMidiVelocity : Dynamics -> Int
toMidiVelocity (Dynamics level) =
    level
        |> clamp 1 127
