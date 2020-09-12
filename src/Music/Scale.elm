module Music.Scale exposing
    ( Scale
    , root, scaleType, toList, containsPitchClass, containsChord, allChords, degree, mode
    , major, minor
    , ionian, dorian, phrygian, lydian, mixolydian, aeolian, locrian
    , melodicMinor, dorianFlat2, lydianAugmented, acoustic, majorMinor, minorLocrian, superlocrian
    , harmonicMinor, locrianNatural6, majorAugmented, lydianDiminished, phrygianDominant, aeolianHarmonic, ultralocrian
    , diminishedWholeToneHalfTone, diminishedHalfToneWholeTone, wholeTone
    , majorPentatonic, minorPentatonic
    , custom
    )

{-| A [scale](https://en.wikipedia.org/wiki/Scale_%28music%29) is a set of pitch classes in a certain order. E.g. the "C major scale."

@docs Scale


# Helpers

@docs root, scaleType, toList, containsPitchClass, containsChord, allChords, degree, mode


# Constructors

@docs major, minor


## Modes of the major scale

@docs ionian, dorian, phrygian, lydian, mixolydian, aeolian, locrian


## Modes of the melodic minor scale

@docs melodicMinor, dorianFlat2, lydianAugmented, acoustic, majorMinor, minorLocrian, superlocrian


## Modes of the harmonic minor scale

@docs harmonicMinor, locrianNatural6, majorAugmented, lydianDiminished, phrygianDominant, aeolianHarmonic, ultralocrian


## Symmetrical scales

@docs diminishedWholeToneHalfTone, diminishedHalfToneWholeTone, wholeTone


## Pentatonic scales

@docs majorPentatonic, minorPentatonic


# Custom scales

@docs custom

-}

import Music.ChordType as ChordType
import Music.Internal.Chord as Chord
import Music.Internal.ChordScale as ChordScale
import Music.Internal.Scale as Scale
import Music.Internal.ScaleType as ScaleType
import Music.PitchClass as PitchClass


{-| -}
type alias Scale =
    Scale.Scale


{-| Get the root pitch class of a scale:

    root (major PitchClass.eFlat) == PitchClass.eFlat

-}
root : Scale -> PitchClass.PitchClass
root theScale =
    Scale.root theScale


{-| Get a [mode](https://en.wikipedia.org/wiki/Mode_%28music%29#Modern_modes) of a scale by shifting the order of its pitch classes. For example, E Phrygian is the third mode of the C major scale:

    mode 3 (major PitchClass.c) == phrygian PitchClass.e

Note: like many numbers in music theory, modes are 1-based; `2` means the second mode of the scale type. 0 and negative integers are ignored.

-}
mode : Int -> Scale -> Scale
mode modeShiftIndex theScale =
    Scale.scale
        (degree modeShiftIndex theScale)
        (ScaleType.mode modeShiftIndex (scaleType theScale))


{-| Get the scale type for a scale:

    scaleType (lydian PitchClass.c) == ScaleType.lydian

-}
scaleType : Scale -> ScaleType.ScaleType
scaleType theScale =
    Scale.scaleType theScale


{-| Get the pitch classes contained in the scale:

    toList (major PitchClass.f)
        == [ PitchClass.f
           , PitchClass.g
           , PitchClass.a
           , PitchClass.bFlat
           , PitchClass.c
           , PitchClass.d
           , PitchClass.e
           ]

-}
toList : Scale -> List PitchClass.PitchClass
toList theScale =
    Scale.toList theScale


{-| Find out whether a scale contains a given pitch class:

    containsPitchClass PitchClass.f
        { ignoreSpelling = False }
        (major PitchClass.c)
        == True

Set the `ignoreSpelling` flag to allow enharmonic equivalents. This returns `True` because G♭ is not in C Lydian, but F♯ (an equivalent) is:

    containsPitchClass PitchClass.gFlat
        { ignoreSpelling = True }
        (lydian PitchClass.c)
        == True

-}
containsPitchClass : PitchClass.PitchClass -> { ignoreSpelling : Bool } -> Scale -> Bool
containsPitchClass pitchClass ignoreSpelling theScale =
    Scale.containsPitchClass pitchClass theScale ignoreSpelling


{-| Find out whether a scale contains a given chord:

    containsChord (Chord.majorSeventhSharpEleven PitchClass.f)
        (major PitchClass.c)
        == True

-}
containsChord : Chord.Chord -> Scale.Scale -> Bool
containsChord chord scale =
    ChordScale.chordIsInScale scale chord


{-| Get all chords contained in a scale, choosing from a list of chord types to detect:

    allChords [ ChordType.minorSeventh ] (major PitchClass.c)
        == [ Chord.minorSeventh PitchClass.d
           , Chord.minorSeventh PitchClass.e
           , Chord.minorSeventh PitchClass.a
           ]

-}
allChords : List ChordType.ChordType -> Scale.Scale -> List Chord.Chord
allChords chordTypesAllowed scale =
    ChordScale.allChordsInScale chordTypesAllowed scale


{-| Get a degree of the scale:

    degree 3 (major PitchClass.c) == PitchClass.e

Note: like many numbers in music theory, degrees are 1-based; `2` means the second degree of the scale. 0 and negative integers are ignored.

-}
degree : Int -> Scale -> PitchClass.PitchClass
degree degreeNumber theScale =
    Scale.degree degreeNumber theScale


{-| -}
ionian : PitchClass.PitchClass -> Scale
ionian scaleRoot =
    Scale.scale scaleRoot ScaleType.ionian


{-| Same as `ionian`
-}
major : PitchClass.PitchClass -> Scale
major scaleRoot =
    Scale.scale scaleRoot ScaleType.major


{-| -}
dorian : PitchClass.PitchClass -> Scale
dorian scaleRoot =
    Scale.scale scaleRoot ScaleType.dorian


{-| -}
phrygian : PitchClass.PitchClass -> Scale
phrygian scaleRoot =
    Scale.scale scaleRoot ScaleType.phrygian


{-| -}
lydian : PitchClass.PitchClass -> Scale
lydian scaleRoot =
    Scale.scale scaleRoot ScaleType.lydian


{-| -}
mixolydian : PitchClass.PitchClass -> Scale
mixolydian scaleRoot =
    Scale.scale scaleRoot ScaleType.mixolydian


{-| -}
aeolian : PitchClass.PitchClass -> Scale
aeolian scaleRoot =
    Scale.scale scaleRoot ScaleType.aeolian


{-| Also known as the "natural minor" scale. Same as `aeolian`
-}
minor : PitchClass.PitchClass -> Scale
minor scaleRoot =
    Scale.scale scaleRoot ScaleType.aeolian


{-| -}
locrian : PitchClass.PitchClass -> Scale
locrian scaleRoot =
    Scale.scale scaleRoot ScaleType.locrian


{-| -}
melodicMinor : PitchClass.PitchClass -> Scale
melodicMinor scaleRoot =
    Scale.scale scaleRoot ScaleType.melodicMinor


{-| -}
dorianFlat2 : PitchClass.PitchClass -> Scale
dorianFlat2 scaleRoot =
    Scale.scale scaleRoot ScaleType.dorianFlat2


{-| -}
lydianAugmented : PitchClass.PitchClass -> Scale
lydianAugmented scaleRoot =
    Scale.scale scaleRoot ScaleType.lydianAugmented


{-| -}
acoustic : PitchClass.PitchClass -> Scale
acoustic scaleRoot =
    Scale.scale scaleRoot ScaleType.acoustic


{-| -}
majorMinor : PitchClass.PitchClass -> Scale
majorMinor scaleRoot =
    Scale.scale scaleRoot ScaleType.majorMinor


{-| -}
minorLocrian : PitchClass.PitchClass -> Scale
minorLocrian scaleRoot =
    Scale.scale scaleRoot ScaleType.minorLocrian


{-| -}
superlocrian : PitchClass.PitchClass -> Scale
superlocrian scaleRoot =
    Scale.scale scaleRoot ScaleType.superlocrian


{-| -}
harmonicMinor : PitchClass.PitchClass -> Scale
harmonicMinor scaleRoot =
    Scale.scale scaleRoot ScaleType.harmonicMinor


{-| -}
locrianNatural6 : PitchClass.PitchClass -> Scale
locrianNatural6 scaleRoot =
    Scale.scale scaleRoot ScaleType.locrianNatural6


{-| -}
majorAugmented : PitchClass.PitchClass -> Scale
majorAugmented scaleRoot =
    Scale.scale scaleRoot ScaleType.majorAugmented


{-| -}
lydianDiminished : PitchClass.PitchClass -> Scale
lydianDiminished scaleRoot =
    Scale.scale scaleRoot ScaleType.lydianDiminished


{-| -}
phrygianDominant : PitchClass.PitchClass -> Scale
phrygianDominant scaleRoot =
    Scale.scale scaleRoot ScaleType.phrygianDominant


{-| -}
aeolianHarmonic : PitchClass.PitchClass -> Scale
aeolianHarmonic scaleRoot =
    Scale.scale scaleRoot ScaleType.aeolianHarmonic


{-| -}
ultralocrian : PitchClass.PitchClass -> Scale
ultralocrian scaleRoot =
    Scale.scale scaleRoot ScaleType.ultralocrian


{-| -}
diminishedWholeToneHalfTone : PitchClass.PitchClass -> Scale
diminishedWholeToneHalfTone scaleRoot =
    Scale.scale scaleRoot ScaleType.diminishedWholeToneHalfTone


{-| -}
diminishedHalfToneWholeTone : PitchClass.PitchClass -> Scale
diminishedHalfToneWholeTone scaleRoot =
    Scale.scale scaleRoot ScaleType.diminishedHalfToneWholeTone


{-| -}
wholeTone : PitchClass.PitchClass -> Scale
wholeTone scaleRoot =
    Scale.scale scaleRoot ScaleType.wholeTone


{-| -}
majorPentatonic : PitchClass.PitchClass -> Scale
majorPentatonic scaleRoot =
    Scale.scale scaleRoot ScaleType.majorPentatonic


{-| -}
minorPentatonic : PitchClass.PitchClass -> Scale
minorPentatonic scaleRoot =
    Scale.scale scaleRoot ScaleType.minorPentatonic


{-| Create a scale from a custom scale type. For use with `ScaleType.customPentatonic`, `ScaleType.customHexatonic`, `ScaleType.customHeptatonic`, and `ScaleType.customOctatonic`:

    myCustomScaleType =
        customPentatonic
            { rootToSecond = Interval.minorThird
            , rootToThird = Interval.perfectFourth
            , rootToFourth = Interval.perfectFifth
            , rootToFifth = Interval.minorSeventh
            }

    myCustomScale =
        |> custom PitchClass.c myCustomScaleType

    -- Equivalent to `minorPentatonic PitchClass.c`

-}
custom : PitchClass.PitchClass -> ScaleType.ScaleType -> Scale
custom pitchClass customScaleType =
    Scale.scale pitchClass customScaleType
