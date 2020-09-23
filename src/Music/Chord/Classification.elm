module Music.Chord.Classification exposing (Classification(..), Triad(..), SixthOrSeventh(..), UnalteredExtension(..), AlteredExtension(..))

{-| This module contains a `Classification` type you can use for making custom views of chord symbols.


# Overview

In my implementation of the `ChordType.toString` function, I've done my best to cover what I consider the usual cases, using chord symbols that are likely to be recognized by a majority of musicians. If `ChordType.toString` works for your purposes, then you don't need this module.

But chord symbols vary greatly with musical style, educational history, and personal preference. The choices I've made may not appeal to you!

Maybe, for example, you want to use modern jazz symbols like "∆7(+11)" and "7(+9-13)". Or maybe you want to write them out in plain English like "dominant seventh (sharp nine, flat thirteen)". You may even want to convert to a more complex view than a `String` can express, like [SVG](https://package.elm-lang.org/packages/elm/svg/latest/) or [elm-ui](https://package.elm-lang.org/packages/mdgriffith/elm-ui/latest/).

If any of these is the case, you can use the `Classification` type to write your own custom chord symbol function:

1.  Call `ChordType.classify` on a `ChordType` to return an instance of `Classification`.
2.  Destructure it with a `case` statement and handle each branch to create your custom view.

A good place to start would be to take a look at the source for `ChordType.toString` to see how I did it.

@docs Classification, Triad, SixthOrSeventh, UnalteredExtension, AlteredExtension

-}

import Music.Interval as Interval


{-| Get an instance of this type by using `ChordType.classify`:

    ChordType.classify ChordType.dominantNinthFlatThirteen
        == Classification MajorTriad (Just MinorSeventh) Ninth [ MinorThirteenth ]

This `Classification` type is my best attempt to codify the chord type taxonomy, as I understand it, into a type you can destructure for your own purposes (i.e. custom views of chord symbols).

Be aware that there are a lot of edge cases in the area of chord classification, and this type can represent some combinations that are unusual, and even impractical, but not clearly invalid.

For instance, I have never seen a "m6(♯11)" chord, but I have also never seen a rule that explains why such a chord cannot be made.

Use your judgment as to which of these you want to handle, and which you want to ignore. Take a look at the source code for `ChordType.toString` to see how I approached this question.

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
