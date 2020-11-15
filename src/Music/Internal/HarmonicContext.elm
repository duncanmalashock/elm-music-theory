module Music.Internal.HarmonicContext exposing
    ( HarmonicContext
    , chord
    , init
    , scale
    )

import Music.Internal.Chord as Chord
import Music.Internal.Scale as Scale


type HarmonicContext
    = HarmonicContext HarmonicContextInput


type alias HarmonicContextInput =
    { chord : Chord.Chord
    , scale : Scale.Scale
    }


init : HarmonicContextInput -> HarmonicContext
init input =
    HarmonicContextInput input.chord input.scale
        |> HarmonicContext


chord : HarmonicContext -> Chord.Chord
chord (HarmonicContext contextData) =
    contextData.chord


scale : HarmonicContext -> Scale.Scale
scale (HarmonicContext contextData) =
    contextData.scale
