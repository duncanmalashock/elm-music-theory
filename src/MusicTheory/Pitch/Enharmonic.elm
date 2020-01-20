module MusicTheory.Pitch.Enharmonic exposing
    ( EnharmonicTransformationError(..)
    , asNaturalOrElseFlat
    , asNaturalOrElseSharp
    , errorToString
    , simple
    )

import MusicTheory.Internal.Pitch as Pitch exposing (Pitch)
import MusicTheory.Internal.PitchClass as PitchClass exposing (PitchClass)
import MusicTheory.Letter as Letter
import MusicTheory.Octave as Octave exposing (OctaveError(..))
import MusicTheory.PitchClass.Enharmonic as Enharmonic


fromPitchClassAndOctaveOffset : Pitch -> ( PitchClass, Int ) -> Result EnharmonicTransformationError Pitch
fromPitchClassAndOctaveOffset pitch ( pc, octaveOffset ) =
    Octave.add octaveOffset (Pitch.octave pitch)
        |> Result.map (\o -> Pitch.fromPitchClass o pc)
        |> Result.mapError (Invalid pc)


simple : Pitch -> Result EnharmonicTransformationError Pitch
simple pitch =
    Enharmonic.simpleWithOctaveOffset (Pitch.pitchClass pitch)
        |> fromPitchClassAndOctaveOffset pitch


asNaturalOrElseSharp : Pitch -> Result EnharmonicTransformationError Pitch
asNaturalOrElseSharp pitch =
    Enharmonic.asNaturalOrElseSharpWithOctaveOffset (Pitch.pitchClass pitch)
        |> fromPitchClassAndOctaveOffset pitch


asNaturalOrElseFlat : Pitch -> Result EnharmonicTransformationError Pitch
asNaturalOrElseFlat pitch =
    Enharmonic.asNaturalOrElseFlatWithOctaveOffset (Pitch.pitchClass pitch)
        |> fromPitchClassAndOctaveOffset pitch


type EnharmonicTransformationError
    = Invalid PitchClass OctaveError


errorToString : EnharmonicTransformationError -> String
errorToString error =
    case error of
        Invalid pc octaveError ->
            let
                letter =
                    PitchClass.letter pc

                offset =
                    PitchClass.offset pc

                accidental =
                    if offset == 0 then
                        " natural"

                    else if offset < 0 then
                        " with " ++ String.fromInt (abs offset) ++ " flats"

                    else
                        " with " ++ String.fromInt (abs offset) ++ " sharps"
            in
            "Pitch class " ++ Letter.toString letter ++ accidental ++ " has an invalid octave. " ++ Octave.errorToString octaveError
