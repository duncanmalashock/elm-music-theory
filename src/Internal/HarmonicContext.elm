module Internal.HarmonicContext exposing
    ( HarmonicContext
    , chord
    , init
    , pitch
    , pitchClass
    , scale
    )

import Internal.Chord as Chord
import Internal.Pitch as Pitch
import Internal.PitchClass as PitchClass
import Internal.Scale as Scale


type HarmonicContext
    = HarmonicContext HarmonicContextInput


type alias HarmonicContextInput =
    { pitch : Pitch.Pitch
    , chord : Chord.Chord
    , scale : Scale.Scale
    }


init : HarmonicContextInput -> HarmonicContext
init input =
    HarmonicContextInput input.pitch input.chord input.scale
        |> HarmonicContext


pitch : HarmonicContext -> Pitch.Pitch
pitch (HarmonicContext contextData) =
    contextData.pitch


pitchClass : HarmonicContext -> PitchClass.PitchClass
pitchClass (HarmonicContext contextData) =
    contextData.pitch
        |> Pitch.pitchClass


chord : HarmonicContext -> Chord.Chord
chord (HarmonicContext contextData) =
    contextData.chord


scale : HarmonicContext -> Scale.Scale
scale (HarmonicContext contextData) =
    contextData.scale
