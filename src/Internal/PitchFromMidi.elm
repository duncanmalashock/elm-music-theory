module Internal.PitchFromMidi exposing (fromMIDINoteNumber)

import Internal.Chromatic
import Internal.Pitch exposing (Pitch)


fromMIDINoteNumber : Int -> Pitch
fromMIDINoteNumber midiNoteNumber =
    let
        matching : Pitch -> Bool
        matching p =
            Internal.Pitch.toMIDINoteNumber p == midiNoteNumber
    in
    Internal.Chromatic.ascending
        |> List.filter matching
        |> List.head
        |> Maybe.withDefault Internal.Pitch.c0
