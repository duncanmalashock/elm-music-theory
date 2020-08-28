module Music.Chord.Classification exposing (Classification(..), TriadClass(..), SixthOrSeventhClass(..), UnalteredExtension(..), AlteredExtension(..))

{-| This module contains a `Classification` type you can use to make custom chord symbols. You probably won't need it, but I have included it to provide more options than `ChordType.symbol` offers.


# Why `Classification`?

Turning chords into symbols is a complicated topic, because:

1.  Since chord classifications mainly exist to describe a small group of the most usual cases, some categories can interact with each other in unspecified ways. For instance, non-tertian "added-tone" chords and chordal "extensions" on 7th chords belong to separate models of chord categorization. Can added-tone chords like m6 have extensions, as in a m6(♭13)? I have personally never seen such a chord, but I have also never seen a rule that says why it cannot be done!
2.  Chord symbol conventions vary with musical idiom, teaching style, and even personal preference.

In my implementation of the `ChordType.symbol` function, I've done my best to cover what I consider the usual cases, with symbols that are likely to be recognized by a majority of musicians. But my choices may not appeal to you!

Maybe, for example, you want to use jazz lead sheet-style symbols like "∆7(+11)" and "7(+9-13)". Or maybe you want to write them out in plain English like "dominant seventh, sharp nine flat thirteen". You may even want to convert to a more complex view than a `String` can express, like SVG or elm-ui.

If that's the case, you can use the `Classification` type to write your own custom chord symbol function. A good place to start would be to take a look at the source for `symbol` to see how I did it.

You may disagree with my interpretation of chord classification too! In that case, you can go as far as writing your own custom classification function using `ChordType.toIntervals` as a starting point.

@docs Classification, TriadClass, SixthOrSeventhClass, UnalteredExtension, AlteredExtension

-}

import Music.Interval as Interval


{-| -}
type Classification
    = Classification TriadClass (Maybe SixthOrSeventhClass) (Maybe UnalteredExtension) (List AlteredExtension)
    | Unclassifiable (List Interval.Interval)


{-| -}
type TriadClass
    = MajorTriad
    | AugmentedTriad
    | MinorTriad
    | DiminishedTriad
    | Sus2Triad
    | Sus4Triad


{-| -}
type SixthOrSeventhClass
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
