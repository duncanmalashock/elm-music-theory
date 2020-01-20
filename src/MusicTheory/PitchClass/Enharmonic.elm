module MusicTheory.PitchClass.Enharmonic exposing
    ( asNaturalOrElseFlat
    , asNaturalOrElseFlatWithOctaveOffset
    , asNaturalOrElseSharp
    , asNaturalOrElseSharpWithOctaveOffset
    , equivalents
    , simple
    , simpleWithOctaveOffset
    )

import MusicTheory.Internal.PitchClass as PitchClass exposing (Offset, PitchClass(..))
import MusicTheory.Internal.PitchClass.Enharmonic as Enharmonic exposing (NaturalOrSingleAccidental(..))


simpleWithOctaveOffset : PitchClass -> ( PitchClass, Int )
simpleWithOctaveOffset pc =
    if PitchClass.offset pc == 0 then
        ( pc, 0 )

    else if PitchClass.offset pc < 0 then
        asNaturalOrElseFlatWithOctaveOffset pc

    else
        asNaturalOrElseSharpWithOctaveOffset pc


simple : PitchClass -> PitchClass
simple =
    simpleWithOctaveOffset >> Tuple.first


asNaturalOrElseFlatWithOctaveOffset : PitchClass -> ( PitchClass, Int )
asNaturalOrElseFlatWithOctaveOffset pc =
    case pc |> PitchClass.semitonesNotOctaveBound |> Enharmonic.semitonesToNaturalOrAccidental 0 of
        Nat letter octaveOffset ->
            ( PitchClass.pitchClass letter PitchClass.natural, octaveOffset )

        SharpFlat _ letter octaveOffset ->
            ( PitchClass.pitchClass letter PitchClass.flat, octaveOffset )


asNaturalOrElseFlat : PitchClass -> PitchClass
asNaturalOrElseFlat =
    asNaturalOrElseFlatWithOctaveOffset >> Tuple.first


asNaturalOrElseSharpWithOctaveOffset : PitchClass -> ( PitchClass, Int )
asNaturalOrElseSharpWithOctaveOffset pc =
    case pc |> PitchClass.semitonesNotOctaveBound |> Enharmonic.semitonesToNaturalOrAccidental 0 of
        Nat letter octaveOffset ->
            ( PitchClass.pitchClass letter PitchClass.natural, octaveOffset )

        SharpFlat letter _ octaveOffset ->
            ( PitchClass.pitchClass letter PitchClass.sharp, octaveOffset )


asNaturalOrElseSharp : PitchClass -> PitchClass
asNaturalOrElseSharp =
    asNaturalOrElseSharpWithOctaveOffset >> Tuple.first


equivalents : PitchClass -> List PitchClass
equivalents pc =
    PitchClass.all |> List.filter (PitchClass.semitones >> (==) (PitchClass.semitones pc))
