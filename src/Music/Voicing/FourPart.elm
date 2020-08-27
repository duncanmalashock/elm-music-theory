module Music.Voicing.FourPart exposing
    ( Voicing, VoicingMethod
    , close, drop2, drop3, drop2and4, spread
    , firstInversion, rootPosition, secondInversion, thirdInversion
    )

{-|

@docs Voicing, VoicingMethod


# Voicing methods


## Jazz

@docs close, drop2, drop3, drop2and4, spread


## Classical

@docs firstInversion, rootPosition, secondInversion, thirdInversion

-}

import Music.Internal.Chord as Chord
import Music.Internal.Voicing.FourPart as FourPart
import Music.Internal.Voicing.FourPart.Classical as FourPartClassical
import Music.Internal.Voicing.FourPart.Jazz as FourPartJazz
import Music.Range as Range


{-| -}
type alias Voicing =
    FourPart.Voicing


{-| -}
type alias VoicingMethod =
    { ranges :
        { voiceOne : Range.Range
        , voiceTwo : Range.Range
        , voiceThree : Range.Range
        , voiceFour : Range.Range
        }
    , chord : Chord.Chord
    }
    -> List Voicing


{-| Voice a chord using the "four-way close" method:

    Chord.voiceFourParts
        { voiceOne = Range.sopranoVoice
        , voiceTwo = Range.altoVoice
        , voiceThree = Range.tenorVoice
        , voiceFour = Range.bassVoice
        }
        [ close ]
        (Chord.majorSeventh PitchClass.c)

-}
close : VoicingMethod
close =
    FourPartJazz.close


{-| -}
drop2 : VoicingMethod
drop2 =
    FourPartJazz.drop2


{-| -}
drop2and4 : VoicingMethod
drop2and4 =
    FourPartJazz.drop2and4


{-| -}
drop3 : VoicingMethod
drop3 =
    FourPartJazz.drop3


{-| -}
spread : VoicingMethod
spread =
    FourPartJazz.spread


{-| -}
rootPosition : VoicingMethod
rootPosition =
    FourPartClassical.rootPosition


{-| -}
firstInversion : VoicingMethod
firstInversion =
    FourPartClassical.firstInversion


{-| -}
secondInversion : VoicingMethod
secondInversion =
    FourPartClassical.secondInversion


{-| -}
thirdInversion : VoicingMethod
thirdInversion =
    FourPartClassical.thirdInversion
