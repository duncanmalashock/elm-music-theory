module Music.Key exposing
    ( Key
    , signature, scale, tonic, isMajor, isMinor, symbol
    , c, f, bFlat, eFlat, aFlat, dFlat, gFlat
    , g, d, a, e, b, fSharp
    , aMinor, dMinor, gMinor, cMinor, fMinor, bFlatMinor, eFlatMinor
    , eMinor, bMinor, fSharpMinor, cSharpMinor, gSharpMinor
    )

{-| A [key](https://en.wikipedia.org/wiki/Key_%28music%29) defines a relationship between a set of pitch classes in a composition.

@docs Key


# Helpers

@docs signature, scale, tonic, isMajor, isMinor, symbol


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


{-| Get the symbol for a key:

    symbol eFlat == "E♭"

    symbol aMinor == "Am"

-}
symbol : Key -> String
symbol key =
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
