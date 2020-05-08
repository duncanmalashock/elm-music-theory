module MusicTheory.Dynamics exposing
    ( Dynamics
    , louder
    , normal
    , softer
    , toMidiVelocity
    )


type Dynamics
    = Dynamics Int


normal : Dynamics
normal =
    Dynamics 0


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
