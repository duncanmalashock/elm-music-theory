module Music.Chord.Classification exposing (Classification(..), Triad(..), SixthOrSeventh(..), UnalteredExtension(..), AlteredExtension(..))

{-| This module contains a `Classification` type you can use to make custom chord symbols. You probably won't need it, but I have included it to provide more options than `ChordType.symbol` offers.

@docs Classification, Triad, SixthOrSeventh, UnalteredExtension, AlteredExtension

-}

import Music.Interval as Interval


{-| Get an instance of this type by using `ChordType.classify`.
-}
type Classification
    = Classification Triad (Maybe SixthOrSeventh) (Maybe UnalteredExtension) (List AlteredExtension)
    | Unclassifiable (List Interval.Interval)


{-| -}
type Triad
    = MajorTriad
    | AugmentedTriad
    | MinorTriad
    | DiminishedTriad
    | Sus2Triad
    | Sus4Triad


{-| -}
type SixthOrSeventh
    = Sixth
    | MajorSeventh
    | MinorSeventh
    | DiminishedSeventh


{-| -}
type UnalteredExtension
    = Ninth
    | Eleventh
    | Thirteenth


{-| -}
type AlteredExtension
    = MinorNinth
    | AugmentedNinth
    | AugmentedEleventh
    | MinorThirteenth
