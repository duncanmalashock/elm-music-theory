module Music.Chord.Classification exposing (Classification(..), Triad(..), SixthOrSeventh(..), UnalteredExtension(..), AlteredExtension(..))

{-| Note: Try using `ChordType.symbol` first. If it works for your purposes, then you don't need this module. It contains a type you can use for making custom chord symbols.


# Disclaimer


## Issues with classifying chords exhaustively

Different chord classification methods have accumulated since at least the 17th century, and they mainly serve to describe the most usual cases in a particular idiom. To my knowledge, there is no unified method for classifying chord types into a taxonomy that is also used by practicing musicians.

This makes classifying chords a complicated topic. Edge cases are everywhere, because some categories that are normally isolated from each other, with only occasional combination, can potentially interact in unspecified ways.

For instance, non-tertian "added-tone" chords and "altered extensions" on 7th chords belong to somewhat separate models of chord categorization, but 6th chords can have 9ths. Does this mean added-tone chords like m6 can have any extensions, as in a m6(♯11)? I have personally never seen any such chord, but I have also never seen a rule that says why it cannot be done!

This `Classification` type is my best attempt to codify the chord type taxonomy, as I understand it, into a type you can destructure for your own purposes (i.e. custom views of chord symbols). Just be aware that, for the reasons I've described, it can represent some combinations that are unusual but not clearly invalid. Use your judgment as to which of these you want to handle, and which you want to ignore.

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
