module MusicTheory.Scale exposing
    ( Scale
    , root
    , scale
    , toList
    )

import MusicTheory.PitchClass as PitchClass exposing (PitchClass)
import MusicTheory.ScaleClass as ScaleClass exposing (ScaleClass)



-- Scale


type Scale
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
    case scaleClass of
        ScaleClass.Pentatonic pentatonicScaleClassIntervals ->
            pentatonicScale scaleRoot pentatonicScaleClassIntervals

        ScaleClass.Hexatonic hexatonicScaleClassIntervals ->
            hexatonicScale scaleRoot hexatonicScaleClassIntervals

        ScaleClass.Heptatonic heptatonicScaleClassIntervals ->
            heptatonicScale scaleRoot heptatonicScaleClassIntervals

        ScaleClass.Octatonic octatonicScaleClassIntervals ->
            octatonicScale scaleRoot octatonicScaleClassIntervals


root : Scale -> PitchClass
root theScale =
    case theScale of
        Pentatonic scaleDegrees ->
            scaleDegrees.root

        Hexatonic scaleDegrees ->
            scaleDegrees.root

        Heptatonic scaleDegrees ->
            scaleDegrees.root

        Octatonic scaleDegrees ->
            scaleDegrees.root


toList : Scale -> List PitchClass
toList theScale =
    case theScale of
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


pentatonicScale : PitchClass -> ScaleClass.PentatonicIntervals -> Scale
pentatonicScale scaleRoot intervals =
    let
        secondDegree =
            PitchClass.transposeUp intervals.first scaleRoot

        thirdDegree =
            PitchClass.transposeUp intervals.second secondDegree

        fourthDegree =
            PitchClass.transposeUp intervals.third thirdDegree

        fifthDegree =
            PitchClass.transposeUp intervals.fourth fourthDegree
    in
    Pentatonic
        { root = scaleRoot
        , second = secondDegree
        , third = thirdDegree
        , fourth = fourthDegree
        , fifth = fifthDegree
        }


hexatonicScale : PitchClass -> ScaleClass.HexatonicIntervals -> Scale
hexatonicScale scaleRoot intervals =
    let
        secondDegree =
            PitchClass.transposeUp intervals.first scaleRoot

        thirdDegree =
            PitchClass.transposeUp intervals.second secondDegree

        fourthDegree =
            PitchClass.transposeUp intervals.third thirdDegree

        fifthDegree =
            PitchClass.transposeUp intervals.fourth fourthDegree

        sixthDegree =
            PitchClass.transposeUp intervals.fifth fifthDegree
    in
    Hexatonic
        { root = scaleRoot
        , second = secondDegree
        , third = thirdDegree
        , fourth = fourthDegree
        , fifth = fifthDegree
        , sixth = sixthDegree
        }


heptatonicScale : PitchClass -> ScaleClass.HeptatonicIntervals -> Scale
heptatonicScale scaleRoot intervals =
    let
        secondDegree =
            PitchClass.transposeUp intervals.first scaleRoot

        thirdDegree =
            PitchClass.transposeUp intervals.second secondDegree

        fourthDegree =
            PitchClass.transposeUp intervals.third thirdDegree

        fifthDegree =
            PitchClass.transposeUp intervals.fourth fourthDegree

        sixthDegree =
            PitchClass.transposeUp intervals.fifth fifthDegree

        seventhDegree =
            PitchClass.transposeUp intervals.sixth sixthDegree
    in
    Heptatonic
        { root = scaleRoot
        , second = secondDegree
        , third = thirdDegree
        , fourth = fourthDegree
        , fifth = fifthDegree
        , sixth = sixthDegree
        , seventh = seventhDegree
        }


octatonicScale : PitchClass -> ScaleClass.OctatonicIntervals -> Scale
octatonicScale scaleRoot intervals =
    let
        secondDegree =
            PitchClass.transposeUp intervals.first scaleRoot

        thirdDegree =
            PitchClass.transposeUp intervals.second secondDegree

        fourthDegree =
            PitchClass.transposeUp intervals.third thirdDegree

        fifthDegree =
            PitchClass.transposeUp intervals.fourth fourthDegree

        sixthDegree =
            PitchClass.transposeUp intervals.fifth fifthDegree

        seventhDegree =
            PitchClass.transposeUp intervals.sixth sixthDegree

        eighthDegree =
            PitchClass.transposeUp intervals.seventh seventhDegree
    in
    Octatonic
        { root = scaleRoot
        , second = secondDegree
        , third = thirdDegree
        , fourth = fourthDegree
        , fifth = fifthDegree
        , sixth = sixthDegree
        , seventh = seventhDegree
        , eighth = eighthDegree
        }
