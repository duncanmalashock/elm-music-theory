module MusicTheory.Dynamics exposing
    ( Dynamics
    , f
    , ff
    , fff
    , louder
    , mf
    , mp
    , normal
    , p
    , pp
    , ppp
    , softer
    , toMidiVelocity
    )


type Dynamics
    = Dynamics Int


normal : Dynamics
normal =
    Dynamics 0


mf : Dynamics
mf =
    Dynamics 1


f : Dynamics
f =
    Dynamics 2


ff : Dynamics
ff =
    Dynamics 3


fff : Dynamics
fff =
    Dynamics 4


mp : Dynamics
mp =
    Dynamics -1


p : Dynamics
p =
    Dynamics -2


pp : Dynamics
pp =
    Dynamics -3


ppp : Dynamics
ppp =
    Dynamics -4


louder : Dynamics -> Dynamics
louder (Dynamics level) =
    Dynamics (level + 1)


softer : Dynamics -> Dynamics
softer (Dynamics level) =
    Dynamics (level - 1)


toMidiVelocity : Dynamics -> Int
toMidiVelocity (Dynamics level) =
    (64 + (level * 16))
        |> clamp 1 127
