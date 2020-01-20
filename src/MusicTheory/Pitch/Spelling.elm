module MusicTheory.Pitch.Spelling exposing
    ( PitchSpelling
    , aFlat
    , aNatural
    , aSharp
    , bFlat
    , bNatural
    , cNatural
    , cSharp
    , dFlat
    , dNatural
    , dSharp
    , eFlat
    , eNatural
    , fNatural
    , fSharp
    , gFlat
    , gNatural
    , gSharp
    , naturalOrElseFlat
    , naturalOrElseSharp
    , simple
    , toPitch
    , toString
    )

import MusicTheory.Internal.Pitch as Pitch exposing (Pitch)
import MusicTheory.Internal.PitchClass as PitchClass exposing (PitchClass)
import MusicTheory.Internal.PitchClass.Enharmonic as PitchClassEnharmonic exposing (NaturalOrSingleAccidental(..))
import MusicTheory.Letter as Letter exposing (Letter(..))
import MusicTheory.Octave as Octave exposing (Octave)
import MusicTheory.Pitch.Enharmonic as Enharmonic exposing (EnharmonicTransformationError(..))
import MusicTheory.PitchClass.Spelling exposing (Accidental(..))


type alias PitchSpelling =
    { letter : Letter
    , accidental : Accidental
    , octave : Octave
    }


simple : Pitch -> Result EnharmonicTransformationError PitchSpelling
simple pitch =
    let
        pitchClass =
            Pitch.pitchClass pitch
    in
    if PitchClass.offset pitchClass == 0 then
        Ok { letter = pitchClass |> PitchClass.letter, accidental = Natural, octave = Pitch.octave pitch }

    else if PitchClass.offset pitchClass < 0 then
        naturalOrElseFlat pitch

    else
        naturalOrElseSharp pitch


toString : PitchSpelling -> String
toString { letter, accidental, octave } =
    Letter.toString letter ++ accidentalToString accidental ++ String.fromInt (Octave.number octave)


toPitch : PitchSpelling -> Pitch
toPitch { letter, accidental, octave } =
    Pitch.pitch letter (accidentalToOffset accidental) octave


naturalOrElseFlat : Pitch -> Result EnharmonicTransformationError PitchSpelling
naturalOrElseFlat pitch =
    let
        pitchClass =
            Pitch.pitchClass pitch
    in
    case pitchClass |> PitchClass.semitonesNotOctaveBound |> PitchClassEnharmonic.semitonesToNaturalOrAccidental 0 of
        Nat letter octaveOffset ->
            Octave.octave (Octave.number (Pitch.octave pitch) + octaveOffset)
                |> Result.map (\octave -> { letter = letter, accidental = Natural, octave = octave })
                |> Result.mapError (Invalid pitchClass)

        SharpFlat _ letter octaveOffset ->
            Octave.octave (Octave.number (Pitch.octave pitch) + octaveOffset)
                |> Result.map (\octave -> { letter = letter, accidental = Flat, octave = octave })
                |> Result.mapError (Invalid pitchClass)


naturalOrElseSharp : Pitch -> Result EnharmonicTransformationError PitchSpelling
naturalOrElseSharp pitch =
    let
        pitchClass =
            Pitch.pitchClass pitch
    in
    case pitchClass |> PitchClass.semitonesNotOctaveBound |> PitchClassEnharmonic.semitonesToNaturalOrAccidental 0 of
        Nat letter octaveOffset ->
            Octave.octave (Octave.number (Pitch.octave pitch) + octaveOffset)
                |> Result.map (\octave -> { letter = letter, accidental = Natural, octave = octave })
                |> Result.mapError (Invalid pitchClass)

        SharpFlat letter _ octaveOffset ->
            Octave.octave (Octave.number (Pitch.octave pitch) + octaveOffset)
                |> Result.map (\octave -> { letter = letter, accidental = Sharp, octave = octave })
                |> Result.mapError (Invalid pitchClass)


cNatural : Octave -> PitchSpelling
cNatural o =
    { letter = C, accidental = Natural, octave = o }


cSharp : Octave -> PitchSpelling
cSharp o =
    { letter = C, accidental = Sharp, octave = o }


dFlat : Octave -> PitchSpelling
dFlat o =
    { letter = D, accidental = Flat, octave = o }


dNatural : Octave -> PitchSpelling
dNatural o =
    { letter = D, accidental = Natural, octave = o }


dSharp : Octave -> PitchSpelling
dSharp o =
    { letter = D, accidental = Sharp, octave = o }


eFlat : Octave -> PitchSpelling
eFlat o =
    { letter = E, accidental = Flat, octave = o }


eNatural : Octave -> PitchSpelling
eNatural o =
    { letter = E, accidental = Natural, octave = o }


fNatural : Octave -> PitchSpelling
fNatural o =
    { letter = F, accidental = Natural, octave = o }


fSharp : Octave -> PitchSpelling
fSharp o =
    { letter = F, accidental = Sharp, octave = o }


gFlat : Octave -> PitchSpelling
gFlat o =
    { letter = G, accidental = Flat, octave = o }


gNatural : Octave -> PitchSpelling
gNatural o =
    { letter = G, accidental = Natural, octave = o }


gSharp : Octave -> PitchSpelling
gSharp o =
    { letter = G, accidental = Sharp, octave = o }


aFlat : Octave -> PitchSpelling
aFlat o =
    { letter = A, accidental = Flat, octave = o }


aNatural : Octave -> PitchSpelling
aNatural o =
    { letter = A, accidental = Natural, octave = o }


aSharp : Octave -> PitchSpelling
aSharp o =
    { letter = A, accidental = Sharp, octave = o }


bFlat : Octave -> PitchSpelling
bFlat o =
    { letter = B, accidental = Flat, octave = o }


bNatural : Octave -> PitchSpelling
bNatural o =
    { letter = B, accidental = Natural, octave = o }



-- INTERNALS


accidentalToOffset : Accidental -> PitchClass.Offset
accidentalToOffset accidental =
    case accidental of
        Flat ->
            Pitch.flat

        Natural ->
            Pitch.natural

        Sharp ->
            Pitch.sharp


accidentalToString : Accidental -> String
accidentalToString accidental =
    case accidental of
        Flat ->
            "♭"

        Natural ->
            ""

        Sharp ->
            "♯"
