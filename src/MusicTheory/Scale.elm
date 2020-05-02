module MusicTheory.Scale exposing
    ( Scale
    , root
    , scale
    , toList
    , toListThroughAllOctaves
    )

import MusicTheory.Octave as Octave
import MusicTheory.Pitch as Pitch exposing (Pitch)
import MusicTheory.PitchClass as PitchClass exposing (PitchClass)
import MusicTheory.ScaleClass as ScaleClass exposing (ScaleClass)


type Scale
    = Scale PitchClass ScaleClass


type ScaleDegrees
    = Pentatonic PentatonicDegrees
    | Hexatonic HexatonicDegrees
    | Heptatonic HeptatonicDegrees
    | Octatonic OctatonicDegrees


type alias PentatonicDegrees =
    { root : PitchClass
    , second : PitchClass
    , third : PitchClass
    , fourth : PitchClass
    , fifth : PitchClass
    }


type alias HexatonicDegrees =
    { root : PitchClass
    , second : PitchClass
    , third : PitchClass
    , fourth : PitchClass
    , fifth : PitchClass
    , sixth : PitchClass
    }


type alias HeptatonicDegrees =
    { root : PitchClass
    , second : PitchClass
    , third : PitchClass
    , fourth : PitchClass
    , fifth : PitchClass
    , sixth : PitchClass
    , seventh : PitchClass
    }


type alias OctatonicDegrees =
    { root : PitchClass
    , second : PitchClass
    , third : PitchClass
    , fourth : PitchClass
    , fifth : PitchClass
    , sixth : PitchClass
    , seventh : PitchClass
    , eighth : PitchClass
    }


scale : PitchClass -> ScaleClass -> Scale
scale scaleRoot scaleClass =
    Scale scaleRoot scaleClass


root : Scale -> PitchClass
root (Scale scaleRoot _) =
    scaleRoot


toListThroughAllOctaves : Scale -> List Pitch
toListThroughAllOctaves (Scale scaleRoot scaleClass) =
    Octave.allValid
        |> List.take 8
        |> List.concatMap
            (\octave ->
                let
                    tonic =
                        Pitch.fromPitchClass octave scaleRoot
                in
                ScaleClass.toList scaleClass
                    |> List.map
                        (\interval ->
                            Pitch.transposeUp interval tonic
                        )
            )


toList : Scale -> List PitchClass
toList theScale =
    case toScaleDegrees theScale of
        Pentatonic scaleDegrees ->
            [ scaleDegrees.root
            , scaleDegrees.second
            , scaleDegrees.third
            , scaleDegrees.fourth
            , scaleDegrees.fifth
            ]

        Hexatonic scaleDegrees ->
            [ scaleDegrees.root
            , scaleDegrees.second
            , scaleDegrees.third
            , scaleDegrees.fourth
            , scaleDegrees.fifth
            , scaleDegrees.sixth
            ]

        Heptatonic scaleDegrees ->
            [ scaleDegrees.root
            , scaleDegrees.second
            , scaleDegrees.third
            , scaleDegrees.fourth
            , scaleDegrees.fifth
            , scaleDegrees.sixth
            , scaleDegrees.seventh
            ]

        Octatonic scaleDegrees ->
            [ scaleDegrees.root
            , scaleDegrees.second
            , scaleDegrees.third
            , scaleDegrees.fourth
            , scaleDegrees.fifth
            , scaleDegrees.sixth
            , scaleDegrees.seventh
            , scaleDegrees.eighth
            ]


toScaleDegrees : Scale -> ScaleDegrees
toScaleDegrees (Scale scaleRoot scaleClass) =
    case scaleClass of
        ScaleClass.Pentatonic scaleClassIntervals ->
            Pentatonic
                { root = scaleRoot
                , second = PitchClass.transpose scaleClassIntervals.rootToSecond scaleRoot
                , third = PitchClass.transpose scaleClassIntervals.rootToThird scaleRoot
                , fourth = PitchClass.transpose scaleClassIntervals.rootToFourth scaleRoot
                , fifth = PitchClass.transpose scaleClassIntervals.rootToFifth scaleRoot
                }

        ScaleClass.Hexatonic scaleClassIntervals ->
            Hexatonic
                { root = scaleRoot
                , second = PitchClass.transpose scaleClassIntervals.rootToSecond scaleRoot
                , third = PitchClass.transpose scaleClassIntervals.rootToThird scaleRoot
                , fourth = PitchClass.transpose scaleClassIntervals.rootToFourth scaleRoot
                , fifth = PitchClass.transpose scaleClassIntervals.rootToFifth scaleRoot
                , sixth = PitchClass.transpose scaleClassIntervals.rootToSixth scaleRoot
                }

        ScaleClass.Heptatonic scaleClassIntervals ->
            Heptatonic
                { root = scaleRoot
                , second = PitchClass.transpose scaleClassIntervals.rootToSecond scaleRoot
                , third = PitchClass.transpose scaleClassIntervals.rootToThird scaleRoot
                , fourth = PitchClass.transpose scaleClassIntervals.rootToFourth scaleRoot
                , fifth = PitchClass.transpose scaleClassIntervals.rootToFifth scaleRoot
                , sixth = PitchClass.transpose scaleClassIntervals.rootToSixth scaleRoot
                , seventh = PitchClass.transpose scaleClassIntervals.rootToSeventh scaleRoot
                }

        ScaleClass.Octatonic scaleClassIntervals ->
            Octatonic
                { root = scaleRoot
                , second = PitchClass.transpose scaleClassIntervals.rootToSecond scaleRoot
                , third = PitchClass.transpose scaleClassIntervals.rootToThird scaleRoot
                , fourth = PitchClass.transpose scaleClassIntervals.rootToFourth scaleRoot
                , fifth = PitchClass.transpose scaleClassIntervals.rootToFifth scaleRoot
                , sixth = PitchClass.transpose scaleClassIntervals.rootToSixth scaleRoot
                , seventh = PitchClass.transpose scaleClassIntervals.rootToSeventh scaleRoot
                , eighth = PitchClass.transpose scaleClassIntervals.rootToEighth scaleRoot
                }
