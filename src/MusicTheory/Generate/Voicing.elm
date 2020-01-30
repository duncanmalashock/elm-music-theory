module MusicTheory.Generate.Voicing exposing (..)

import MusicTheory.Chord as Chord
import MusicTheory.Pitch as Pitch
import MusicTheory.PitchClass as PitchClass


rootPosition : Chord.Chord -> List PitchClass.PitchClass
rootPosition chord =
    -- TODO: use constraint solver to convert to `List PitchClass`
    Chord.toTertianFactors chord
        |> (\tf ->
                [ [ Just tf.root ]
                , [ Just tf.thirdOrSus ]
                , List.map Just tf.fifth
                , [ tf.seventh ]
                , List.map Just tf.ninth
                , [ tf.eleventh ]
                , [ tf.thirteenthOrSixth ]
                ]
           )
        |> List.concat
        |> List.filterMap identity
