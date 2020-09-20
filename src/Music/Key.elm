module Music.Key exposing
    ( Key
    , tonic, scale, isMajor, isMinor, signature
    , relative, parallel
    , toString
    , c, f, bFlat, eFlat, aFlat, dFlat, gFlat
    , g, d, a, e, b, fSharp
    , aMinor, dMinor, gMinor, cMinor, fMinor, bFlatMinor, eFlatMinor
    , eMinor, bMinor, fSharpMinor, cSharpMinor, gSharpMinor
    )

{-| A [key](https://en.wikipedia.org/wiki/Key_%28music%29) defines a relationship between a set of pitch classes in a composition. E.g. the "key of D major".

@docs Key


# Helpers

@docs tonic, scale, isMajor, isMinor, signature


# Related keys

@docs relative, parallel


# Conversion

@docs toString


# Constructors


## Major keys

@docs c, f, bFlat, eFlat, aFlat, dFlat, gFlat
@docs g, d, a, e, b, fSharp


## Minor keys

@docs aMinor, dMinor, gMinor, cMinor, fMinor, bFlatMinor, eFlatMinor
@docs eMinor, bMinor, fSharpMinor, cSharpMinor, gSharpMinor

-}

import Music.Internal.Key as Key
import Music.Internal.Scale as Scale
import Music.PitchClass as PitchClass


{-| -}
type alias Key =
    Key.Key


{-| Get the [key signature](https://en.wikipedia.org/wiki/Key_signature) for a key as a list of the pitch classes that are sharpened or flattened:

    signature bFlat == [ PitchClass.bFlat, PitchClass.eFlat ]

-}
signature : Key -> List PitchClass.PitchClass
signature key =
    Key.signature key


{-| Get the name of a key:

    toString eFlat == "E♭"

    toString aMinor == "Am"

-}
toString : Key -> String
toString key =
    Key.symbol key


{-| Check whether a key is major:

    isMajor bFlat == True

-}
isMajor : Key -> Bool
isMajor key =
    Key.isMajor key


{-| Check whether a key is minor:

    isMinor cSharpMinor == True

-}
isMinor : Key -> Bool
isMinor key =
    Key.isMajor key


{-| Get the scale for a key:

    scale aFlat == Scale.major PitchClass.aFlat

-}
scale : Key -> Scale.Scale
scale key =
    Key.scale key


{-| Get the [relative minor or relative major](https://en.wikipedia.org/wiki/Relative_key) of a key:

    relative f == dMinor

    relative cSharpMinor == e

-}
relative : Key -> Key
relative key =
    Key.relative key


{-| Get the [parallel minor or parallel major](https://en.wikipedia.org/wiki/Parallel_key) of a key:

    parallel f == fMinor

    parallel cSharpMinor == cSharp

-}
parallel : Key -> Key
parallel key =
    Key.parallel key


{-| Get the tonic pitch class for a key:

    tonic gMinor == PitchClass.g

-}
tonic : Key -> PitchClass.PitchClass
tonic key =
    Key.tonic key



---- MAJOR KEYS


{-| -}
gFlat : Key
gFlat =
    Key.gFlat


{-| -}
dFlat : Key
dFlat =
    Key.dFlat


{-| -}
aFlat : Key
aFlat =
    Key.aFlat


{-| -}
eFlat : Key
eFlat =
    Key.eFlat


{-| -}
bFlat : Key
bFlat =
    Key.bFlat


{-| -}
f : Key
f =
    Key.f


{-| -}
c : Key
c =
    Key.c


{-| -}
g : Key
g =
    Key.g


{-| -}
d : Key
d =
    Key.d


{-| -}
a : Key
a =
    Key.a


{-| -}
e : Key
e =
    Key.e


{-| -}
b : Key
b =
    Key.b


{-| -}
fSharp : Key
fSharp =
    Key.fSharp



---- MINOR KEYS


{-| -}
eFlatMinor : Key
eFlatMinor =
    Key.eFlatMinor


{-| -}
bFlatMinor : Key
bFlatMinor =
    Key.bFlatMinor


{-| -}
fMinor : Key
fMinor =
    Key.fMinor


{-| -}
cMinor : Key
cMinor =
    Key.cMinor


{-| -}
gMinor : Key
gMinor =
    Key.gMinor


{-| -}
dMinor : Key
dMinor =
    Key.dMinor


{-| -}
aMinor : Key
aMinor =
    Key.aMinor


{-| -}
eMinor : Key
eMinor =
    Key.eMinor


{-| -}
bMinor : Key
bMinor =
    Key.bMinor


{-| -}
fSharpMinor : Key
fSharpMinor =
    Key.fSharpMinor


{-| -}
cSharpMinor : Key
cSharpMinor =
    Key.cSharpMinor


{-| -}
gSharpMinor : Key
gSharpMinor =
    Key.gSharpMinor
