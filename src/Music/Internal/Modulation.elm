module Music.Internal.Modulation exposing
    ( Modulation
    , apply
    , downByFifths
    , upByFifths
    )

import Music.Internal.Interval as Interval
import Music.Internal.Key as Key
import Music.Internal.PitchClass as PitchClass
import Util.Basic as Util


type Modulation
    = Modulation ModulationDetails


type alias ModulationDetails =
    { stepsToRotate : Int
    , changeMode : Bool
    , normalizeSettings : NormalizeSettings
    }


type NormalizeSettings
    = DontNormalize
    | NormalizeAfter6Accidentals
    | NormalizeAt6Accidentals PreferGFlatOrFSharp


type PreferGFlatOrFSharp
    = PreferGFlat
    | PreferFSharp


upByFifths : Int -> Modulation
upByFifths numFifths =
    Modulation
        { stepsToRotate = numFifths
        , changeMode = False
        , normalizeSettings = DontNormalize
        }


downByFifths : Int -> Modulation
downByFifths numFifths =
    Modulation
        { stepsToRotate = -1 * numFifths
        , changeMode = False
        , normalizeSettings = DontNormalize
        }


apply : Modulation -> Key.Key -> Key.Key
apply (Modulation details) key =
    key
        |> (if details.stepsToRotate >= 0 then
                Util.applyNTimes
                    details.stepsToRotate
                    nextKeyUpAFifth

            else
                Util.applyNTimes
                    (details.stepsToRotate * -1)
                    nextKeyDownAFifth
           )


nextKeyDownAFifth : Key.Key -> Key.Key
nextKeyDownAFifth key =
    Key.setTonic
        (Key.tonic key
            |> PitchClass.transpose Interval.perfectFourth
        )
        key


nextKeyUpAFifth : Key.Key -> Key.Key
nextKeyUpAFifth key =
    Key.setTonic
        (Key.tonic key
            |> PitchClass.transpose Interval.perfectFifth
        )
        key
