module MusicTheory.Pitch exposing
    ( c0, c1, c2, c3, c4, c5, c6, c7, c8
    , cFlat1, cFlat2, cFlat3, cFlat4, cFlat5, cFlat6, cFlat7, cFlat8
    , cSharp0, cSharp1, cSharp2, cSharp3, cSharp4, cSharp5, cSharp6, cSharp7, cSharp8
    , d0, d1, d2, d3, d4, d5, d6, d7, d8
    , dFlat0, dFlat1, dFlat2, dFlat3, dFlat4, dFlat5, dFlat6, dFlat7, dFlat8
    , dSharp0, dSharp1, dSharp2, dSharp3, dSharp4, dSharp5, dSharp6, dSharp7, dSharp8
    , e0, e1, e2, e3, e4, e5, e6, e7, e8
    , eFlat0, eFlat1, eFlat2, eFlat3, eFlat4, eFlat5, eFlat6, eFlat7, eFlat8
    , eSharp0, eSharp1, eSharp2, eSharp3, eSharp4, eSharp5, eSharp6, eSharp7, eSharp8
    , f0, f1, f2, f3, f4, f5, f6, f7, f8
    , fFlat0, fFlat1, fFlat2, fFlat3, fFlat4, fFlat5, fFlat6, fFlat7, fFlat8
    , fSharp0, fSharp1, fSharp2, fSharp3, fSharp4, fSharp5, fSharp6, fSharp7, fSharp8
    , g0, g1, g2, g3, g4, g5, g6, g7, g8
    , gFlat0, gFlat1, gFlat2, gFlat3, gFlat4, gFlat5, gFlat6, gFlat7, gFlat8
    , gSharp0, gSharp1, gSharp2, gSharp3, gSharp4, gSharp5, gSharp6, gSharp7, gSharp8
    , a0, a1, a2, a3, a4, a5, a6, a7, a8
    , aFlat0, aFlat1, aFlat2, aFlat3, aFlat4, aFlat5, aFlat6, aFlat7, aFlat8
    , aSharp0, aSharp1, aSharp2, aSharp3, aSharp4, aSharp5, aSharp6, aSharp7, aSharp8
    , b0, b1, b2, b3, b4, b5, b6, b7, b8
    , bFlat0, bFlat1, bFlat2, bFlat3, bFlat4, bFlat5, bFlat6, bFlat7, bFlat8
    , bSharp0, bSharp1, bSharp2, bSharp3, bSharp4, bSharp5, bSharp6, bSharp7, bSharp8
    )

{-| A pitch represents a specific frequency of sound with a letter name, octave, and accidental.


# Pitch constructors

@docs c0, c1, c2, c3, c4, c5, c6, c7, c8
@docs cFlat1, cFlat2, cFlat3, cFlat4, cFlat5, cFlat6, cFlat7, cFlat8
@docs cSharp0, cSharp1, cSharp2, cSharp3, cSharp4, cSharp5, cSharp6, cSharp7, cSharp8
@docs d0, d1, d2, d3, d4, d5, d6, d7, d8
@docs dFlat0, dFlat1, dFlat2, dFlat3, dFlat4, dFlat5, dFlat6, dFlat7, dFlat8
@docs dSharp0, dSharp1, dSharp2, dSharp3, dSharp4, dSharp5, dSharp6, dSharp7, dSharp8
@docs e0, e1, e2, e3, e4, e5, e6, e7, e8
@docs eFlat0, eFlat1, eFlat2, eFlat3, eFlat4, eFlat5, eFlat6, eFlat7, eFlat8
@docs eSharp0, eSharp1, eSharp2, eSharp3, eSharp4, eSharp5, eSharp6, eSharp7, eSharp8
@docs f0, f1, f2, f3, f4, f5, f6, f7, f8
@docs fFlat0, fFlat1, fFlat2, fFlat3, fFlat4, fFlat5, fFlat6, fFlat7, fFlat8
@docs fSharp0, fSharp1, fSharp2, fSharp3, fSharp4, fSharp5, fSharp6, fSharp7, fSharp8
@docs g0, g1, g2, g3, g4, g5, g6, g7, g8
@docs gFlat0, gFlat1, gFlat2, gFlat3, gFlat4, gFlat5, gFlat6, gFlat7, gFlat8
@docs gSharp0, gSharp1, gSharp2, gSharp3, gSharp4, gSharp5, gSharp6, gSharp7, gSharp8
@docs a0, a1, a2, a3, a4, a5, a6, a7, a8
@docs aFlat0, aFlat1, aFlat2, aFlat3, aFlat4, aFlat5, aFlat6, aFlat7, aFlat8
@docs aSharp0, aSharp1, aSharp2, aSharp3, aSharp4, aSharp5, aSharp6, aSharp7, aSharp8
@docs b0, b1, b2, b3, b4, b5, b6, b7, b8
@docs bFlat0, bFlat1, bFlat2, bFlat3, bFlat4, bFlat5, bFlat6, bFlat7, bFlat8
@docs bSharp0, bSharp1, bSharp2, bSharp3, bSharp4, bSharp5, bSharp6, bSharp7, bSharp8

-}

import MusicTheory.Internal.Pitch as Pitch



-- Pitch constructors


{-| The pitch c0.
-}
c0 : Pitch.Pitch
c0 =
    Pitch.c0


{-| The pitch cSharp0.
-}
cSharp0 : Pitch.Pitch
cSharp0 =
    Pitch.cSharp0


{-| The pitch d0.
-}
d0 : Pitch.Pitch
d0 =
    Pitch.d0


{-| The pitch dSharp0.
-}
dSharp0 : Pitch.Pitch
dSharp0 =
    Pitch.dSharp0


{-| The pitch dFlat0.
-}
dFlat0 : Pitch.Pitch
dFlat0 =
    Pitch.dFlat0


{-| The pitch e0.
-}
e0 : Pitch.Pitch
e0 =
    Pitch.e0


{-| The pitch eSharp0.
-}
eSharp0 : Pitch.Pitch
eSharp0 =
    Pitch.eSharp0


{-| The pitch eFlat0.
-}
eFlat0 : Pitch.Pitch
eFlat0 =
    Pitch.eFlat0


{-| The pitch f0.
-}
f0 : Pitch.Pitch
f0 =
    Pitch.f0


{-| The pitch fSharp0.
-}
fSharp0 : Pitch.Pitch
fSharp0 =
    Pitch.fSharp0


{-| The pitch fFlat0.
-}
fFlat0 : Pitch.Pitch
fFlat0 =
    Pitch.fFlat0


{-| The pitch g0.
-}
g0 : Pitch.Pitch
g0 =
    Pitch.g0


{-| The pitch gSharp0.
-}
gSharp0 : Pitch.Pitch
gSharp0 =
    Pitch.gSharp0


{-| The pitch gFlat0.
-}
gFlat0 : Pitch.Pitch
gFlat0 =
    Pitch.gFlat0


{-| The pitch a0.
-}
a0 : Pitch.Pitch
a0 =
    Pitch.a0


{-| The pitch aSharp0.
-}
aSharp0 : Pitch.Pitch
aSharp0 =
    Pitch.aSharp0


{-| The pitch aFlat0.
-}
aFlat0 : Pitch.Pitch
aFlat0 =
    Pitch.aFlat0


{-| The pitch b0.
-}
b0 : Pitch.Pitch
b0 =
    Pitch.b0


{-| The pitch bSharp0.
-}
bSharp0 : Pitch.Pitch
bSharp0 =
    Pitch.bSharp0


{-| The pitch bFlat0.
-}
bFlat0 : Pitch.Pitch
bFlat0 =
    Pitch.bFlat0


{-| The pitch c1.
-}
c1 : Pitch.Pitch
c1 =
    Pitch.c1


{-| The pitch cSharp1.
-}
cSharp1 : Pitch.Pitch
cSharp1 =
    Pitch.cSharp1


{-| The pitch cFlat1.
-}
cFlat1 : Pitch.Pitch
cFlat1 =
    Pitch.cFlat1


{-| The pitch d1.
-}
d1 : Pitch.Pitch
d1 =
    Pitch.d1


{-| The pitch dSharp1.
-}
dSharp1 : Pitch.Pitch
dSharp1 =
    Pitch.dSharp1


{-| The pitch dFlat1.
-}
dFlat1 : Pitch.Pitch
dFlat1 =
    Pitch.dFlat1


{-| The pitch e1.
-}
e1 : Pitch.Pitch
e1 =
    Pitch.e1


{-| The pitch eSharp1.
-}
eSharp1 : Pitch.Pitch
eSharp1 =
    Pitch.eSharp1


{-| The pitch eFlat1.
-}
eFlat1 : Pitch.Pitch
eFlat1 =
    Pitch.eFlat1


{-| The pitch f1.
-}
f1 : Pitch.Pitch
f1 =
    Pitch.f1


{-| The pitch fSharp1.
-}
fSharp1 : Pitch.Pitch
fSharp1 =
    Pitch.fSharp1


{-| The pitch fFlat1.
-}
fFlat1 : Pitch.Pitch
fFlat1 =
    Pitch.fFlat1


{-| The pitch g1.
-}
g1 : Pitch.Pitch
g1 =
    Pitch.g1


{-| The pitch gSharp1.
-}
gSharp1 : Pitch.Pitch
gSharp1 =
    Pitch.gSharp1


{-| The pitch gFlat1.
-}
gFlat1 : Pitch.Pitch
gFlat1 =
    Pitch.gFlat1


{-| The pitch a1.
-}
a1 : Pitch.Pitch
a1 =
    Pitch.a1


{-| The pitch aSharp1.
-}
aSharp1 : Pitch.Pitch
aSharp1 =
    Pitch.aSharp1


{-| The pitch aFlat1.
-}
aFlat1 : Pitch.Pitch
aFlat1 =
    Pitch.aFlat1


{-| The pitch b1.
-}
b1 : Pitch.Pitch
b1 =
    Pitch.b1


{-| The pitch bSharp1.
-}
bSharp1 : Pitch.Pitch
bSharp1 =
    Pitch.bSharp1


{-| The pitch bFlat1.
-}
bFlat1 : Pitch.Pitch
bFlat1 =
    Pitch.bFlat1


{-| The pitch c2.
-}
c2 : Pitch.Pitch
c2 =
    Pitch.c2


{-| The pitch cSharp2.
-}
cSharp2 : Pitch.Pitch
cSharp2 =
    Pitch.cSharp2


{-| The pitch cFlat2.
-}
cFlat2 : Pitch.Pitch
cFlat2 =
    Pitch.cFlat2


{-| The pitch d2.
-}
d2 : Pitch.Pitch
d2 =
    Pitch.d2


{-| The pitch dSharp2.
-}
dSharp2 : Pitch.Pitch
dSharp2 =
    Pitch.dSharp2


{-| The pitch dFlat2.
-}
dFlat2 : Pitch.Pitch
dFlat2 =
    Pitch.dFlat2


{-| The pitch e2.
-}
e2 : Pitch.Pitch
e2 =
    Pitch.e2


{-| The pitch eSharp2.
-}
eSharp2 : Pitch.Pitch
eSharp2 =
    Pitch.eSharp2


{-| The pitch eFlat2.
-}
eFlat2 : Pitch.Pitch
eFlat2 =
    Pitch.eFlat2


{-| The pitch f2.
-}
f2 : Pitch.Pitch
f2 =
    Pitch.f2


{-| The pitch fSharp2.
-}
fSharp2 : Pitch.Pitch
fSharp2 =
    Pitch.fSharp2


{-| The pitch fFlat2.
-}
fFlat2 : Pitch.Pitch
fFlat2 =
    Pitch.fFlat2


{-| The pitch g2.
-}
g2 : Pitch.Pitch
g2 =
    Pitch.g2


{-| The pitch gSharp2.
-}
gSharp2 : Pitch.Pitch
gSharp2 =
    Pitch.gSharp2


{-| The pitch gFlat2.
-}
gFlat2 : Pitch.Pitch
gFlat2 =
    Pitch.gFlat2


{-| The pitch a2.
-}
a2 : Pitch.Pitch
a2 =
    Pitch.a2


{-| The pitch aSharp2.
-}
aSharp2 : Pitch.Pitch
aSharp2 =
    Pitch.aSharp2


{-| The pitch aFlat2.
-}
aFlat2 : Pitch.Pitch
aFlat2 =
    Pitch.aFlat2


{-| The pitch b2.
-}
b2 : Pitch.Pitch
b2 =
    Pitch.b2


{-| The pitch bSharp2.
-}
bSharp2 : Pitch.Pitch
bSharp2 =
    Pitch.bSharp2


{-| The pitch bFlat2.
-}
bFlat2 : Pitch.Pitch
bFlat2 =
    Pitch.bFlat2


{-| The pitch c3.
-}
c3 : Pitch.Pitch
c3 =
    Pitch.c3


{-| The pitch cSharp3.
-}
cSharp3 : Pitch.Pitch
cSharp3 =
    Pitch.cSharp3


{-| The pitch cFlat3.
-}
cFlat3 : Pitch.Pitch
cFlat3 =
    Pitch.cFlat3


{-| The pitch d3.
-}
d3 : Pitch.Pitch
d3 =
    Pitch.d3


{-| The pitch dSharp3.
-}
dSharp3 : Pitch.Pitch
dSharp3 =
    Pitch.dSharp3


{-| The pitch dFlat3.
-}
dFlat3 : Pitch.Pitch
dFlat3 =
    Pitch.dFlat3


{-| The pitch e3.
-}
e3 : Pitch.Pitch
e3 =
    Pitch.e3


{-| The pitch eSharp3.
-}
eSharp3 : Pitch.Pitch
eSharp3 =
    Pitch.eSharp3


{-| The pitch eFlat3.
-}
eFlat3 : Pitch.Pitch
eFlat3 =
    Pitch.eFlat3


{-| The pitch f3.
-}
f3 : Pitch.Pitch
f3 =
    Pitch.f3


{-| The pitch fSharp3.
-}
fSharp3 : Pitch.Pitch
fSharp3 =
    Pitch.fSharp3


{-| The pitch fFlat3.
-}
fFlat3 : Pitch.Pitch
fFlat3 =
    Pitch.fFlat3


{-| The pitch g3.
-}
g3 : Pitch.Pitch
g3 =
    Pitch.g3


{-| The pitch gSharp3.
-}
gSharp3 : Pitch.Pitch
gSharp3 =
    Pitch.gSharp3


{-| The pitch gFlat3.
-}
gFlat3 : Pitch.Pitch
gFlat3 =
    Pitch.gFlat3


{-| The pitch a3.
-}
a3 : Pitch.Pitch
a3 =
    Pitch.a3


{-| The pitch aSharp3.
-}
aSharp3 : Pitch.Pitch
aSharp3 =
    Pitch.aSharp3


{-| The pitch aFlat3.
-}
aFlat3 : Pitch.Pitch
aFlat3 =
    Pitch.aFlat3


{-| The pitch b3.
-}
b3 : Pitch.Pitch
b3 =
    Pitch.b3


{-| The pitch bSharp3.
-}
bSharp3 : Pitch.Pitch
bSharp3 =
    Pitch.bSharp3


{-| The pitch bFlat3.
-}
bFlat3 : Pitch.Pitch
bFlat3 =
    Pitch.bFlat3


{-| The pitch c4.
-}
c4 : Pitch.Pitch
c4 =
    Pitch.c4


{-| The pitch cSharp4.
-}
cSharp4 : Pitch.Pitch
cSharp4 =
    Pitch.cSharp4


{-| The pitch cFlat4.
-}
cFlat4 : Pitch.Pitch
cFlat4 =
    Pitch.cFlat4


{-| The pitch d4.
-}
d4 : Pitch.Pitch
d4 =
    Pitch.d4


{-| The pitch dSharp4.
-}
dSharp4 : Pitch.Pitch
dSharp4 =
    Pitch.dSharp4


{-| The pitch dFlat4.
-}
dFlat4 : Pitch.Pitch
dFlat4 =
    Pitch.dFlat4


{-| The pitch e4.
-}
e4 : Pitch.Pitch
e4 =
    Pitch.e4


{-| The pitch eSharp4.
-}
eSharp4 : Pitch.Pitch
eSharp4 =
    Pitch.eSharp4


{-| The pitch eFlat4.
-}
eFlat4 : Pitch.Pitch
eFlat4 =
    Pitch.eFlat4


{-| The pitch f4.
-}
f4 : Pitch.Pitch
f4 =
    Pitch.f4


{-| The pitch fSharp4.
-}
fSharp4 : Pitch.Pitch
fSharp4 =
    Pitch.fSharp4


{-| The pitch fFlat4.
-}
fFlat4 : Pitch.Pitch
fFlat4 =
    Pitch.fFlat4


{-| The pitch g4.
-}
g4 : Pitch.Pitch
g4 =
    Pitch.g4


{-| The pitch gSharp4.
-}
gSharp4 : Pitch.Pitch
gSharp4 =
    Pitch.gSharp4


{-| The pitch gFlat4.
-}
gFlat4 : Pitch.Pitch
gFlat4 =
    Pitch.gFlat4


{-| The pitch a4.
-}
a4 : Pitch.Pitch
a4 =
    Pitch.a4


{-| The pitch aSharp4.
-}
aSharp4 : Pitch.Pitch
aSharp4 =
    Pitch.aSharp4


{-| The pitch aFlat4.
-}
aFlat4 : Pitch.Pitch
aFlat4 =
    Pitch.aFlat4


{-| The pitch b4.
-}
b4 : Pitch.Pitch
b4 =
    Pitch.b4


{-| The pitch bSharp4.
-}
bSharp4 : Pitch.Pitch
bSharp4 =
    Pitch.bSharp4


{-| The pitch bFlat4.
-}
bFlat4 : Pitch.Pitch
bFlat4 =
    Pitch.bFlat4


{-| The pitch c5.
-}
c5 : Pitch.Pitch
c5 =
    Pitch.c5


{-| The pitch cSharp5.
-}
cSharp5 : Pitch.Pitch
cSharp5 =
    Pitch.cSharp5


{-| The pitch cFlat5.
-}
cFlat5 : Pitch.Pitch
cFlat5 =
    Pitch.cFlat5


{-| The pitch d5.
-}
d5 : Pitch.Pitch
d5 =
    Pitch.d5


{-| The pitch dSharp5.
-}
dSharp5 : Pitch.Pitch
dSharp5 =
    Pitch.dSharp5


{-| The pitch dFlat5.
-}
dFlat5 : Pitch.Pitch
dFlat5 =
    Pitch.dFlat5


{-| The pitch e5.
-}
e5 : Pitch.Pitch
e5 =
    Pitch.e5


{-| The pitch eSharp5.
-}
eSharp5 : Pitch.Pitch
eSharp5 =
    Pitch.eSharp5


{-| The pitch eFlat5.
-}
eFlat5 : Pitch.Pitch
eFlat5 =
    Pitch.eFlat5


{-| The pitch f5.
-}
f5 : Pitch.Pitch
f5 =
    Pitch.f5


{-| The pitch fSharp5.
-}
fSharp5 : Pitch.Pitch
fSharp5 =
    Pitch.fSharp5


{-| The pitch fFlat5.
-}
fFlat5 : Pitch.Pitch
fFlat5 =
    Pitch.fFlat5


{-| The pitch g5.
-}
g5 : Pitch.Pitch
g5 =
    Pitch.g5


{-| The pitch gSharp5.
-}
gSharp5 : Pitch.Pitch
gSharp5 =
    Pitch.gSharp5


{-| The pitch gFlat5.
-}
gFlat5 : Pitch.Pitch
gFlat5 =
    Pitch.gFlat5


{-| The pitch a5.
-}
a5 : Pitch.Pitch
a5 =
    Pitch.a5


{-| The pitch aSharp5.
-}
aSharp5 : Pitch.Pitch
aSharp5 =
    Pitch.aSharp5


{-| The pitch aFlat5.
-}
aFlat5 : Pitch.Pitch
aFlat5 =
    Pitch.aFlat5


{-| The pitch b5.
-}
b5 : Pitch.Pitch
b5 =
    Pitch.b5


{-| The pitch bSharp5.
-}
bSharp5 : Pitch.Pitch
bSharp5 =
    Pitch.bSharp5


{-| The pitch bFlat5.
-}
bFlat5 : Pitch.Pitch
bFlat5 =
    Pitch.bFlat5


{-| The pitch c6.
-}
c6 : Pitch.Pitch
c6 =
    Pitch.c6


{-| The pitch cSharp6.
-}
cSharp6 : Pitch.Pitch
cSharp6 =
    Pitch.cSharp6


{-| The pitch cFlat6.
-}
cFlat6 : Pitch.Pitch
cFlat6 =
    Pitch.cFlat6


{-| The pitch d6.
-}
d6 : Pitch.Pitch
d6 =
    Pitch.d6


{-| The pitch dSharp6.
-}
dSharp6 : Pitch.Pitch
dSharp6 =
    Pitch.dSharp6


{-| The pitch dFlat6.
-}
dFlat6 : Pitch.Pitch
dFlat6 =
    Pitch.dFlat6


{-| The pitch e6.
-}
e6 : Pitch.Pitch
e6 =
    Pitch.e6


{-| The pitch eSharp6.
-}
eSharp6 : Pitch.Pitch
eSharp6 =
    Pitch.eSharp6


{-| The pitch eFlat6.
-}
eFlat6 : Pitch.Pitch
eFlat6 =
    Pitch.eFlat6


{-| The pitch f6.
-}
f6 : Pitch.Pitch
f6 =
    Pitch.f6


{-| The pitch fSharp6.
-}
fSharp6 : Pitch.Pitch
fSharp6 =
    Pitch.fSharp6


{-| The pitch fFlat6.
-}
fFlat6 : Pitch.Pitch
fFlat6 =
    Pitch.fFlat6


{-| The pitch g6.
-}
g6 : Pitch.Pitch
g6 =
    Pitch.g6


{-| The pitch gSharp6.
-}
gSharp6 : Pitch.Pitch
gSharp6 =
    Pitch.gSharp6


{-| The pitch gFlat6.
-}
gFlat6 : Pitch.Pitch
gFlat6 =
    Pitch.gFlat6


{-| The pitch a6.
-}
a6 : Pitch.Pitch
a6 =
    Pitch.a6


{-| The pitch aSharp6.
-}
aSharp6 : Pitch.Pitch
aSharp6 =
    Pitch.aSharp6


{-| The pitch aFlat6.
-}
aFlat6 : Pitch.Pitch
aFlat6 =
    Pitch.aFlat6


{-| The pitch b6.
-}
b6 : Pitch.Pitch
b6 =
    Pitch.b6


{-| The pitch bSharp6.
-}
bSharp6 : Pitch.Pitch
bSharp6 =
    Pitch.bSharp6


{-| The pitch bFlat6.
-}
bFlat6 : Pitch.Pitch
bFlat6 =
    Pitch.bFlat6


{-| The pitch c7.
-}
c7 : Pitch.Pitch
c7 =
    Pitch.c7


{-| The pitch cSharp7.
-}
cSharp7 : Pitch.Pitch
cSharp7 =
    Pitch.cSharp7


{-| The pitch cFlat7.
-}
cFlat7 : Pitch.Pitch
cFlat7 =
    Pitch.cFlat7


{-| The pitch d7.
-}
d7 : Pitch.Pitch
d7 =
    Pitch.d7


{-| The pitch dSharp7.
-}
dSharp7 : Pitch.Pitch
dSharp7 =
    Pitch.dSharp7


{-| The pitch dFlat7.
-}
dFlat7 : Pitch.Pitch
dFlat7 =
    Pitch.dFlat7


{-| The pitch e7.
-}
e7 : Pitch.Pitch
e7 =
    Pitch.e7


{-| The pitch eSharp7.
-}
eSharp7 : Pitch.Pitch
eSharp7 =
    Pitch.eSharp7


{-| The pitch eFlat7.
-}
eFlat7 : Pitch.Pitch
eFlat7 =
    Pitch.eFlat7


{-| The pitch f7.
-}
f7 : Pitch.Pitch
f7 =
    Pitch.f7


{-| The pitch fSharp7.
-}
fSharp7 : Pitch.Pitch
fSharp7 =
    Pitch.fSharp7


{-| The pitch fFlat7.
-}
fFlat7 : Pitch.Pitch
fFlat7 =
    Pitch.fFlat7


{-| The pitch g7.
-}
g7 : Pitch.Pitch
g7 =
    Pitch.g7


{-| The pitch gSharp7.
-}
gSharp7 : Pitch.Pitch
gSharp7 =
    Pitch.gSharp7


{-| The pitch gFlat7.
-}
gFlat7 : Pitch.Pitch
gFlat7 =
    Pitch.gFlat7


{-| The pitch a7.
-}
a7 : Pitch.Pitch
a7 =
    Pitch.a7


{-| The pitch aSharp7.
-}
aSharp7 : Pitch.Pitch
aSharp7 =
    Pitch.aSharp7


{-| The pitch aFlat7.
-}
aFlat7 : Pitch.Pitch
aFlat7 =
    Pitch.aFlat7


{-| The pitch b7.
-}
b7 : Pitch.Pitch
b7 =
    Pitch.b7


{-| The pitch bSharp7.
-}
bSharp7 : Pitch.Pitch
bSharp7 =
    Pitch.bSharp7


{-| The pitch bFlat7.
-}
bFlat7 : Pitch.Pitch
bFlat7 =
    Pitch.bFlat7


{-| The pitch c8.
-}
c8 : Pitch.Pitch
c8 =
    Pitch.c8


{-| The pitch cSharp8.
-}
cSharp8 : Pitch.Pitch
cSharp8 =
    Pitch.cSharp8


{-| The pitch cFlat8.
-}
cFlat8 : Pitch.Pitch
cFlat8 =
    Pitch.cFlat8


{-| The pitch d8.
-}
d8 : Pitch.Pitch
d8 =
    Pitch.d8


{-| The pitch dSharp8.
-}
dSharp8 : Pitch.Pitch
dSharp8 =
    Pitch.dSharp8


{-| The pitch dFlat8.
-}
dFlat8 : Pitch.Pitch
dFlat8 =
    Pitch.dFlat8


{-| The pitch e8.
-}
e8 : Pitch.Pitch
e8 =
    Pitch.e8


{-| The pitch eSharp8.
-}
eSharp8 : Pitch.Pitch
eSharp8 =
    Pitch.eSharp8


{-| The pitch eFlat8.
-}
eFlat8 : Pitch.Pitch
eFlat8 =
    Pitch.eFlat8


{-| The pitch f8.
-}
f8 : Pitch.Pitch
f8 =
    Pitch.f8


{-| The pitch fSharp8.
-}
fSharp8 : Pitch.Pitch
fSharp8 =
    Pitch.fSharp8


{-| The pitch fFlat8.
-}
fFlat8 : Pitch.Pitch
fFlat8 =
    Pitch.fFlat8


{-| The pitch g8.
-}
g8 : Pitch.Pitch
g8 =
    Pitch.g8


{-| The pitch gSharp8.
-}
gSharp8 : Pitch.Pitch
gSharp8 =
    Pitch.gSharp8


{-| The pitch gFlat8.
-}
gFlat8 : Pitch.Pitch
gFlat8 =
    Pitch.gFlat8


{-| The pitch a8.
-}
a8 : Pitch.Pitch
a8 =
    Pitch.a8


{-| The pitch aSharp8.
-}
aSharp8 : Pitch.Pitch
aSharp8 =
    Pitch.aSharp8


{-| The pitch aFlat8.
-}
aFlat8 : Pitch.Pitch
aFlat8 =
    Pitch.aFlat8


{-| The pitch b8.
-}
b8 : Pitch.Pitch
b8 =
    Pitch.b8


{-| The pitch bSharp8.
-}
bSharp8 : Pitch.Pitch
bSharp8 =
    Pitch.bSharp8


{-| The pitch bFlat8.
-}
bFlat8 : Pitch.Pitch
bFlat8 =
    Pitch.bFlat8
