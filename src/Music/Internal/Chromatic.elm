module Music.Internal.Chromatic exposing (ascending, descending)

import Music.Internal.Octave as Octave
import Music.Internal.Pitch as Pitch
import Music.PitchClass as PitchClass


ascending : List Pitch.Pitch
ascending =
    Octave.allValid
        |> List.concatMap
            (\octave ->
                pitchClassesAscending
                    |> List.map
                        (\pitchClass ->
                            Pitch.fromPitchClass octave pitchClass
                        )
            )


descending : List Pitch.Pitch
descending =
    Octave.allValid
        |> List.reverse
        |> List.concatMap
            (\octave ->
                pitchClassesDescending
                    |> List.map
                        (\pitchClass ->
                            Pitch.fromPitchClass octave pitchClass
                        )
            )


pitchClassesAscending : List PitchClass.PitchClass
pitchClassesAscending =
    [ PitchClass.c
    , PitchClass.cSharp
    , PitchClass.d
    , PitchClass.dSharp
    , PitchClass.e
    , PitchClass.f
    , PitchClass.fSharp
    , PitchClass.g
    , PitchClass.gSharp
    , PitchClass.a
    , PitchClass.aSharp
    , PitchClass.b
    ]


pitchClassesDescending : List PitchClass.PitchClass
pitchClassesDescending =
    [ PitchClass.b
    , PitchClass.bFlat
    , PitchClass.a
    , PitchClass.aFlat
    , PitchClass.g
    , PitchClass.gFlat
    , PitchClass.f
    , PitchClass.e
    , PitchClass.eFlat
    , PitchClass.d
    , PitchClass.dFlat
    , PitchClass.c
    ]
