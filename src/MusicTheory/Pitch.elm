module MusicTheory.Pitch exposing
    ( Pitch
    , PitchError(..)
    , a0
    , a1
    , a2
    , a3
    , a4
    , a5
    , a6
    , a7
    , a8
    , aFlat0
    , aFlat1
    , aFlat2
    , aFlat3
    , aFlat4
    , aFlat5
    , aFlat6
    , aFlat7
    , aFlat8
    , aSharp0
    , aSharp1
    , aSharp2
    , aSharp3
    , aSharp4
    , aSharp5
    , aSharp6
    , aSharp7
    , aSharp8
    , all
    , allForPitchClass
    , areEnharmonicEquivalents
    , b0
    , b1
    , b2
    , b3
    , b4
    , b5
    , b6
    , b7
    , b8
    , bFlat0
    , bFlat1
    , bFlat2
    , bFlat3
    , bFlat4
    , bFlat5
    , bFlat6
    , bFlat7
    , bFlat8
    , bSharp0
    , bSharp1
    , bSharp2
    , bSharp3
    , bSharp4
    , bSharp5
    , bSharp6
    , bSharp7
    , bSharp8
    , c0
    , c1
    , c2
    , c3
    , c4
    , c5
    , c6
    , c7
    , c8
    , cFlat1
    , cFlat2
    , cFlat3
    , cFlat4
    , cFlat5
    , cFlat6
    , cFlat7
    , cFlat8
    , cSharp0
    , cSharp1
    , cSharp2
    , cSharp3
    , cSharp4
    , cSharp5
    , cSharp6
    , cSharp7
    , cSharp8
    , d0
    , d1
    , d2
    , d3
    , d4
    , d5
    , d6
    , d7
    , d8
    , dFlat0
    , dFlat1
    , dFlat2
    , dFlat3
    , dFlat4
    , dFlat5
    , dFlat6
    , dFlat7
    , dFlat8
    , dSharp0
    , dSharp1
    , dSharp2
    , dSharp3
    , dSharp4
    , dSharp5
    , dSharp6
    , dSharp7
    , dSharp8
    , doubleFlat
    , doubleSharp
    , e0
    , e1
    , e2
    , e3
    , e4
    , e5
    , e6
    , e7
    , e8
    , eFlat0
    , eFlat1
    , eFlat2
    , eFlat3
    , eFlat4
    , eFlat5
    , eFlat6
    , eFlat7
    , eFlat8
    , eSharp0
    , eSharp1
    , eSharp2
    , eSharp3
    , eSharp4
    , eSharp5
    , eSharp6
    , eSharp7
    , eSharp8
    , errorToString
    , f0
    , f1
    , f2
    , f3
    , f4
    , f5
    , f6
    , f7
    , f8
    , fFlat0
    , fFlat1
    , fFlat2
    , fFlat3
    , fFlat4
    , fFlat5
    , fFlat6
    , fFlat7
    , fFlat8
    , fSharp0
    , fSharp1
    , fSharp2
    , fSharp3
    , fSharp4
    , fSharp5
    , fSharp6
    , fSharp7
    , fSharp8
    , firstAbove
    , firstBelow
    , flat
    , fromPitchClass
    , g0
    , g1
    , g2
    , g3
    , g4
    , g5
    , g6
    , g7
    , g8
    , gFlat0
    , gFlat1
    , gFlat2
    , gFlat3
    , gFlat4
    , gFlat5
    , gFlat6
    , gFlat7
    , gFlat8
    , gSharp0
    , gSharp1
    , gSharp2
    , gSharp3
    , gSharp4
    , gSharp5
    , gSharp6
    , gSharp7
    , gSharp8
    , natural
    , octave
    , pitch
    , pitchClass
    , semitones
    , sharp
    , transposeDown
    , transposeUp
    , tripleFlat
    , tripleSharp
    )

import MusicTheory.Interval as Interval exposing (Interval)
import MusicTheory.Letter exposing (Letter(..))
import MusicTheory.Octave as Octave exposing (Octave, OctaveError(..))
import MusicTheory.PitchClass as PitchClass exposing (Offset, PitchClass)


semitonesUpperLimit : Int
semitonesUpperLimit =
    120


semitonesLowerLimit : Int
semitonesLowerLimit =
    0


c0 : Pitch
c0 =
    Pitch (PitchClass.pitchClass C natural) Octave.zero


cSharp0 : Pitch
cSharp0 =
    Pitch (PitchClass.pitchClass C sharp) Octave.zero


d0 : Pitch
d0 =
    Pitch (PitchClass.pitchClass D natural) Octave.zero


dSharp0 : Pitch
dSharp0 =
    Pitch (PitchClass.pitchClass D sharp) Octave.zero


dFlat0 : Pitch
dFlat0 =
    Pitch (PitchClass.pitchClass D flat) Octave.zero


e0 : Pitch
e0 =
    Pitch (PitchClass.pitchClass E natural) Octave.zero


eSharp0 : Pitch
eSharp0 =
    Pitch (PitchClass.pitchClass E sharp) Octave.zero


eFlat0 : Pitch
eFlat0 =
    Pitch (PitchClass.pitchClass E flat) Octave.zero


f0 : Pitch
f0 =
    Pitch (PitchClass.pitchClass F natural) Octave.zero


fSharp0 : Pitch
fSharp0 =
    Pitch (PitchClass.pitchClass F sharp) Octave.zero


fFlat0 : Pitch
fFlat0 =
    Pitch (PitchClass.pitchClass F flat) Octave.zero


g0 : Pitch
g0 =
    Pitch (PitchClass.pitchClass G natural) Octave.zero


gSharp0 : Pitch
gSharp0 =
    Pitch (PitchClass.pitchClass G sharp) Octave.zero


gFlat0 : Pitch
gFlat0 =
    Pitch (PitchClass.pitchClass G flat) Octave.zero


a0 : Pitch
a0 =
    Pitch (PitchClass.pitchClass A natural) Octave.zero


aSharp0 : Pitch
aSharp0 =
    Pitch (PitchClass.pitchClass A sharp) Octave.zero


aFlat0 : Pitch
aFlat0 =
    Pitch (PitchClass.pitchClass A flat) Octave.zero


b0 : Pitch
b0 =
    Pitch (PitchClass.pitchClass B natural) Octave.zero


bSharp0 : Pitch
bSharp0 =
    Pitch (PitchClass.pitchClass B sharp) Octave.zero


bFlat0 : Pitch
bFlat0 =
    Pitch (PitchClass.pitchClass B flat) Octave.zero


c1 : Pitch
c1 =
    Pitch (PitchClass.pitchClass C natural) Octave.one


cSharp1 : Pitch
cSharp1 =
    Pitch (PitchClass.pitchClass C sharp) Octave.one


cFlat1 : Pitch
cFlat1 =
    Pitch (PitchClass.pitchClass C flat) Octave.one


d1 : Pitch
d1 =
    Pitch (PitchClass.pitchClass D natural) Octave.one


dSharp1 : Pitch
dSharp1 =
    Pitch (PitchClass.pitchClass D sharp) Octave.one


dFlat1 : Pitch
dFlat1 =
    Pitch (PitchClass.pitchClass D flat) Octave.one


e1 : Pitch
e1 =
    Pitch (PitchClass.pitchClass E natural) Octave.one


eSharp1 : Pitch
eSharp1 =
    Pitch (PitchClass.pitchClass E sharp) Octave.one


eFlat1 : Pitch
eFlat1 =
    Pitch (PitchClass.pitchClass E flat) Octave.one


f1 : Pitch
f1 =
    Pitch (PitchClass.pitchClass F natural) Octave.one


fSharp1 : Pitch
fSharp1 =
    Pitch (PitchClass.pitchClass F sharp) Octave.one


fFlat1 : Pitch
fFlat1 =
    Pitch (PitchClass.pitchClass F flat) Octave.one


g1 : Pitch
g1 =
    Pitch (PitchClass.pitchClass G natural) Octave.one


gSharp1 : Pitch
gSharp1 =
    Pitch (PitchClass.pitchClass G sharp) Octave.one


gFlat1 : Pitch
gFlat1 =
    Pitch (PitchClass.pitchClass G flat) Octave.one


a1 : Pitch
a1 =
    Pitch (PitchClass.pitchClass A natural) Octave.one


aSharp1 : Pitch
aSharp1 =
    Pitch (PitchClass.pitchClass A sharp) Octave.one


aFlat1 : Pitch
aFlat1 =
    Pitch (PitchClass.pitchClass A flat) Octave.one


b1 : Pitch
b1 =
    Pitch (PitchClass.pitchClass B natural) Octave.one


bSharp1 : Pitch
bSharp1 =
    Pitch (PitchClass.pitchClass B sharp) Octave.one


bFlat1 : Pitch
bFlat1 =
    Pitch (PitchClass.pitchClass B flat) Octave.one


c2 : Pitch
c2 =
    Pitch (PitchClass.pitchClass C natural) Octave.two


cSharp2 : Pitch
cSharp2 =
    Pitch (PitchClass.pitchClass C sharp) Octave.two


cFlat2 : Pitch
cFlat2 =
    Pitch (PitchClass.pitchClass C flat) Octave.two


d2 : Pitch
d2 =
    Pitch (PitchClass.pitchClass D natural) Octave.two


dSharp2 : Pitch
dSharp2 =
    Pitch (PitchClass.pitchClass D sharp) Octave.two


dFlat2 : Pitch
dFlat2 =
    Pitch (PitchClass.pitchClass D flat) Octave.two


e2 : Pitch
e2 =
    Pitch (PitchClass.pitchClass E natural) Octave.two


eSharp2 : Pitch
eSharp2 =
    Pitch (PitchClass.pitchClass E sharp) Octave.two


eFlat2 : Pitch
eFlat2 =
    Pitch (PitchClass.pitchClass E flat) Octave.two


f2 : Pitch
f2 =
    Pitch (PitchClass.pitchClass F natural) Octave.two


fSharp2 : Pitch
fSharp2 =
    Pitch (PitchClass.pitchClass F sharp) Octave.two


fFlat2 : Pitch
fFlat2 =
    Pitch (PitchClass.pitchClass F flat) Octave.two


g2 : Pitch
g2 =
    Pitch (PitchClass.pitchClass G natural) Octave.two


gSharp2 : Pitch
gSharp2 =
    Pitch (PitchClass.pitchClass G sharp) Octave.two


gFlat2 : Pitch
gFlat2 =
    Pitch (PitchClass.pitchClass G flat) Octave.two


a2 : Pitch
a2 =
    Pitch (PitchClass.pitchClass A natural) Octave.two


aSharp2 : Pitch
aSharp2 =
    Pitch (PitchClass.pitchClass A sharp) Octave.two


aFlat2 : Pitch
aFlat2 =
    Pitch (PitchClass.pitchClass A flat) Octave.two


b2 : Pitch
b2 =
    Pitch (PitchClass.pitchClass B natural) Octave.two


bSharp2 : Pitch
bSharp2 =
    Pitch (PitchClass.pitchClass B sharp) Octave.two


bFlat2 : Pitch
bFlat2 =
    Pitch (PitchClass.pitchClass B flat) Octave.two


c3 : Pitch
c3 =
    Pitch (PitchClass.pitchClass C natural) Octave.three


cSharp3 : Pitch
cSharp3 =
    Pitch (PitchClass.pitchClass C sharp) Octave.three


cFlat3 : Pitch
cFlat3 =
    Pitch (PitchClass.pitchClass C flat) Octave.three


d3 : Pitch
d3 =
    Pitch (PitchClass.pitchClass D natural) Octave.three


dSharp3 : Pitch
dSharp3 =
    Pitch (PitchClass.pitchClass D sharp) Octave.three


dFlat3 : Pitch
dFlat3 =
    Pitch (PitchClass.pitchClass D flat) Octave.three


e3 : Pitch
e3 =
    Pitch (PitchClass.pitchClass E natural) Octave.three


eSharp3 : Pitch
eSharp3 =
    Pitch (PitchClass.pitchClass E sharp) Octave.three


eFlat3 : Pitch
eFlat3 =
    Pitch (PitchClass.pitchClass E flat) Octave.three


f3 : Pitch
f3 =
    Pitch (PitchClass.pitchClass F natural) Octave.three


fSharp3 : Pitch
fSharp3 =
    Pitch (PitchClass.pitchClass F sharp) Octave.three


fFlat3 : Pitch
fFlat3 =
    Pitch (PitchClass.pitchClass F flat) Octave.three


g3 : Pitch
g3 =
    Pitch (PitchClass.pitchClass G natural) Octave.three


gSharp3 : Pitch
gSharp3 =
    Pitch (PitchClass.pitchClass G sharp) Octave.three


gFlat3 : Pitch
gFlat3 =
    Pitch (PitchClass.pitchClass G flat) Octave.three


a3 : Pitch
a3 =
    Pitch (PitchClass.pitchClass A natural) Octave.three


aSharp3 : Pitch
aSharp3 =
    Pitch (PitchClass.pitchClass A sharp) Octave.three


aFlat3 : Pitch
aFlat3 =
    Pitch (PitchClass.pitchClass A flat) Octave.three


b3 : Pitch
b3 =
    Pitch (PitchClass.pitchClass B natural) Octave.three


bSharp3 : Pitch
bSharp3 =
    Pitch (PitchClass.pitchClass B sharp) Octave.three


bFlat3 : Pitch
bFlat3 =
    Pitch (PitchClass.pitchClass B flat) Octave.three


c4 : Pitch
c4 =
    Pitch (PitchClass.pitchClass C natural) Octave.four


cSharp4 : Pitch
cSharp4 =
    Pitch (PitchClass.pitchClass C sharp) Octave.four


cFlat4 : Pitch
cFlat4 =
    Pitch (PitchClass.pitchClass C flat) Octave.four


d4 : Pitch
d4 =
    Pitch (PitchClass.pitchClass D natural) Octave.four


dSharp4 : Pitch
dSharp4 =
    Pitch (PitchClass.pitchClass D sharp) Octave.four


dFlat4 : Pitch
dFlat4 =
    Pitch (PitchClass.pitchClass D flat) Octave.four


e4 : Pitch
e4 =
    Pitch (PitchClass.pitchClass E natural) Octave.four


eSharp4 : Pitch
eSharp4 =
    Pitch (PitchClass.pitchClass E sharp) Octave.four


eFlat4 : Pitch
eFlat4 =
    Pitch (PitchClass.pitchClass E flat) Octave.four


f4 : Pitch
f4 =
    Pitch (PitchClass.pitchClass F natural) Octave.four


fSharp4 : Pitch
fSharp4 =
    Pitch (PitchClass.pitchClass F sharp) Octave.four


fFlat4 : Pitch
fFlat4 =
    Pitch (PitchClass.pitchClass F flat) Octave.four


g4 : Pitch
g4 =
    Pitch (PitchClass.pitchClass G natural) Octave.four


gSharp4 : Pitch
gSharp4 =
    Pitch (PitchClass.pitchClass G sharp) Octave.four


gFlat4 : Pitch
gFlat4 =
    Pitch (PitchClass.pitchClass G flat) Octave.four


a4 : Pitch
a4 =
    Pitch (PitchClass.pitchClass A natural) Octave.four


aSharp4 : Pitch
aSharp4 =
    Pitch (PitchClass.pitchClass A sharp) Octave.four


aFlat4 : Pitch
aFlat4 =
    Pitch (PitchClass.pitchClass A flat) Octave.four


b4 : Pitch
b4 =
    Pitch (PitchClass.pitchClass B natural) Octave.four


bSharp4 : Pitch
bSharp4 =
    Pitch (PitchClass.pitchClass B sharp) Octave.four


bFlat4 : Pitch
bFlat4 =
    Pitch (PitchClass.pitchClass B flat) Octave.four


c5 : Pitch
c5 =
    Pitch (PitchClass.pitchClass C natural) Octave.five


cSharp5 : Pitch
cSharp5 =
    Pitch (PitchClass.pitchClass C sharp) Octave.five


cFlat5 : Pitch
cFlat5 =
    Pitch (PitchClass.pitchClass C flat) Octave.five


d5 : Pitch
d5 =
    Pitch (PitchClass.pitchClass D natural) Octave.five


dSharp5 : Pitch
dSharp5 =
    Pitch (PitchClass.pitchClass D sharp) Octave.five


dFlat5 : Pitch
dFlat5 =
    Pitch (PitchClass.pitchClass D flat) Octave.five


e5 : Pitch
e5 =
    Pitch (PitchClass.pitchClass E natural) Octave.five


eSharp5 : Pitch
eSharp5 =
    Pitch (PitchClass.pitchClass E sharp) Octave.five


eFlat5 : Pitch
eFlat5 =
    Pitch (PitchClass.pitchClass E flat) Octave.five


f5 : Pitch
f5 =
    Pitch (PitchClass.pitchClass F natural) Octave.five


fSharp5 : Pitch
fSharp5 =
    Pitch (PitchClass.pitchClass F sharp) Octave.five


fFlat5 : Pitch
fFlat5 =
    Pitch (PitchClass.pitchClass F flat) Octave.five


g5 : Pitch
g5 =
    Pitch (PitchClass.pitchClass G natural) Octave.five


gSharp5 : Pitch
gSharp5 =
    Pitch (PitchClass.pitchClass G sharp) Octave.five


gFlat5 : Pitch
gFlat5 =
    Pitch (PitchClass.pitchClass G flat) Octave.five


a5 : Pitch
a5 =
    Pitch (PitchClass.pitchClass A natural) Octave.five


aSharp5 : Pitch
aSharp5 =
    Pitch (PitchClass.pitchClass A sharp) Octave.five


aFlat5 : Pitch
aFlat5 =
    Pitch (PitchClass.pitchClass A flat) Octave.five


b5 : Pitch
b5 =
    Pitch (PitchClass.pitchClass B natural) Octave.five


bSharp5 : Pitch
bSharp5 =
    Pitch (PitchClass.pitchClass B sharp) Octave.five


bFlat5 : Pitch
bFlat5 =
    Pitch (PitchClass.pitchClass B flat) Octave.five


c6 : Pitch
c6 =
    Pitch (PitchClass.pitchClass C natural) Octave.six


cSharp6 : Pitch
cSharp6 =
    Pitch (PitchClass.pitchClass C sharp) Octave.six


cFlat6 : Pitch
cFlat6 =
    Pitch (PitchClass.pitchClass C flat) Octave.six


d6 : Pitch
d6 =
    Pitch (PitchClass.pitchClass D natural) Octave.six


dSharp6 : Pitch
dSharp6 =
    Pitch (PitchClass.pitchClass D sharp) Octave.six


dFlat6 : Pitch
dFlat6 =
    Pitch (PitchClass.pitchClass D flat) Octave.six


e6 : Pitch
e6 =
    Pitch (PitchClass.pitchClass E natural) Octave.six


eSharp6 : Pitch
eSharp6 =
    Pitch (PitchClass.pitchClass E sharp) Octave.six


eFlat6 : Pitch
eFlat6 =
    Pitch (PitchClass.pitchClass E flat) Octave.six


f6 : Pitch
f6 =
    Pitch (PitchClass.pitchClass F natural) Octave.six


fSharp6 : Pitch
fSharp6 =
    Pitch (PitchClass.pitchClass F sharp) Octave.six


fFlat6 : Pitch
fFlat6 =
    Pitch (PitchClass.pitchClass F flat) Octave.six


g6 : Pitch
g6 =
    Pitch (PitchClass.pitchClass G natural) Octave.six


gSharp6 : Pitch
gSharp6 =
    Pitch (PitchClass.pitchClass G sharp) Octave.six


gFlat6 : Pitch
gFlat6 =
    Pitch (PitchClass.pitchClass G flat) Octave.six


a6 : Pitch
a6 =
    Pitch (PitchClass.pitchClass A natural) Octave.six


aSharp6 : Pitch
aSharp6 =
    Pitch (PitchClass.pitchClass A sharp) Octave.six


aFlat6 : Pitch
aFlat6 =
    Pitch (PitchClass.pitchClass A flat) Octave.six


b6 : Pitch
b6 =
    Pitch (PitchClass.pitchClass B natural) Octave.six


bSharp6 : Pitch
bSharp6 =
    Pitch (PitchClass.pitchClass B sharp) Octave.six


bFlat6 : Pitch
bFlat6 =
    Pitch (PitchClass.pitchClass B flat) Octave.six


c7 : Pitch
c7 =
    Pitch (PitchClass.pitchClass C natural) Octave.seven


cSharp7 : Pitch
cSharp7 =
    Pitch (PitchClass.pitchClass C sharp) Octave.seven


cFlat7 : Pitch
cFlat7 =
    Pitch (PitchClass.pitchClass C flat) Octave.seven


d7 : Pitch
d7 =
    Pitch (PitchClass.pitchClass D natural) Octave.seven


dSharp7 : Pitch
dSharp7 =
    Pitch (PitchClass.pitchClass D sharp) Octave.seven


dFlat7 : Pitch
dFlat7 =
    Pitch (PitchClass.pitchClass D flat) Octave.seven


e7 : Pitch
e7 =
    Pitch (PitchClass.pitchClass E natural) Octave.seven


eSharp7 : Pitch
eSharp7 =
    Pitch (PitchClass.pitchClass E sharp) Octave.seven


eFlat7 : Pitch
eFlat7 =
    Pitch (PitchClass.pitchClass E flat) Octave.seven


f7 : Pitch
f7 =
    Pitch (PitchClass.pitchClass F natural) Octave.seven


fSharp7 : Pitch
fSharp7 =
    Pitch (PitchClass.pitchClass F sharp) Octave.seven


fFlat7 : Pitch
fFlat7 =
    Pitch (PitchClass.pitchClass F flat) Octave.seven


g7 : Pitch
g7 =
    Pitch (PitchClass.pitchClass G natural) Octave.seven


gSharp7 : Pitch
gSharp7 =
    Pitch (PitchClass.pitchClass G sharp) Octave.seven


gFlat7 : Pitch
gFlat7 =
    Pitch (PitchClass.pitchClass G flat) Octave.seven


a7 : Pitch
a7 =
    Pitch (PitchClass.pitchClass A natural) Octave.seven


aSharp7 : Pitch
aSharp7 =
    Pitch (PitchClass.pitchClass A sharp) Octave.seven


aFlat7 : Pitch
aFlat7 =
    Pitch (PitchClass.pitchClass A flat) Octave.seven


b7 : Pitch
b7 =
    Pitch (PitchClass.pitchClass B natural) Octave.seven


bSharp7 : Pitch
bSharp7 =
    Pitch (PitchClass.pitchClass B sharp) Octave.seven


bFlat7 : Pitch
bFlat7 =
    Pitch (PitchClass.pitchClass B flat) Octave.seven


c8 : Pitch
c8 =
    Pitch (PitchClass.pitchClass C natural) Octave.eight


cSharp8 : Pitch
cSharp8 =
    Pitch (PitchClass.pitchClass C sharp) Octave.eight


cFlat8 : Pitch
cFlat8 =
    Pitch (PitchClass.pitchClass C flat) Octave.eight


d8 : Pitch
d8 =
    Pitch (PitchClass.pitchClass D natural) Octave.eight


dSharp8 : Pitch
dSharp8 =
    Pitch (PitchClass.pitchClass D sharp) Octave.eight


dFlat8 : Pitch
dFlat8 =
    Pitch (PitchClass.pitchClass D flat) Octave.eight


e8 : Pitch
e8 =
    Pitch (PitchClass.pitchClass E natural) Octave.eight


eSharp8 : Pitch
eSharp8 =
    Pitch (PitchClass.pitchClass E sharp) Octave.eight


eFlat8 : Pitch
eFlat8 =
    Pitch (PitchClass.pitchClass E flat) Octave.eight


f8 : Pitch
f8 =
    Pitch (PitchClass.pitchClass F natural) Octave.eight


fSharp8 : Pitch
fSharp8 =
    Pitch (PitchClass.pitchClass F sharp) Octave.eight


fFlat8 : Pitch
fFlat8 =
    Pitch (PitchClass.pitchClass F flat) Octave.eight


g8 : Pitch
g8 =
    Pitch (PitchClass.pitchClass G natural) Octave.eight


gSharp8 : Pitch
gSharp8 =
    Pitch (PitchClass.pitchClass G sharp) Octave.eight


gFlat8 : Pitch
gFlat8 =
    Pitch (PitchClass.pitchClass G flat) Octave.eight


a8 : Pitch
a8 =
    Pitch (PitchClass.pitchClass A natural) Octave.eight


aSharp8 : Pitch
aSharp8 =
    Pitch (PitchClass.pitchClass A sharp) Octave.eight


aFlat8 : Pitch
aFlat8 =
    Pitch (PitchClass.pitchClass A flat) Octave.eight


b8 : Pitch
b8 =
    Pitch (PitchClass.pitchClass B natural) Octave.eight


bSharp8 : Pitch
bSharp8 =
    Pitch (PitchClass.pitchClass B sharp) Octave.eight


bFlat8 : Pitch
bFlat8 =
    Pitch (PitchClass.pitchClass B flat) Octave.eight


type Pitch
    = Pitch PitchClass Octave


type PitchError
    = InvalidOctave OctaveError
    | SemitonesOutOfRange Int
    | ValidPitchNotFound
    | InternalError


pitch : Letter -> Offset -> Octave -> Result PitchError Pitch
pitch l os o =
    fromPitchClass o (PitchClass.pitchClass l os)


tripleFlat : Offset
tripleFlat =
    PitchClass.tripleFlat


doubleFlat : Offset
doubleFlat =
    PitchClass.doubleFlat


flat : Offset
flat =
    PitchClass.flat


natural : Offset
natural =
    PitchClass.natural


sharp : Offset
sharp =
    PitchClass.sharp


doubleSharp : Offset
doubleSharp =
    PitchClass.doubleSharp


tripleSharp : Offset
tripleSharp =
    PitchClass.tripleSharp


pitchClass : Pitch -> PitchClass
pitchClass (Pitch pc _) =
    pc


octave : Pitch -> Octave
octave (Pitch _ o) =
    o


fromPitchClass : Octave -> PitchClass -> Result PitchError Pitch
fromPitchClass o p =
    let
        newPitch =
            Pitch p o
    in
    if
        (semitones newPitch > semitonesUpperLimit)
            || (semitones newPitch < semitonesLowerLimit)
    then
        Err <| SemitonesOutOfRange (semitones newPitch)

    else
        Ok newPitch


semitones : Pitch -> Int
semitones (Pitch pc o) =
    Octave.semitones o + PitchClass.semitones pc


areEnharmonicEquivalents : Pitch -> Pitch -> Bool
areEnharmonicEquivalents lhs rhs =
    semitones lhs == semitones rhs


all : List Pitch
all =
    Octave.all
        |> List.concatMap
            (\o ->
                PitchClass.all
                    |> List.map (\pc -> Pitch pc o)
            )


allForPitchClass : PitchClass -> List Pitch
allForPitchClass thePitchClass =
    Octave.all
        |> List.map
            (\theOctave ->
                fromPitchClass theOctave thePitchClass
            )
        |> List.filterMap Result.toMaybe


firstBelow : PitchClass -> Pitch -> Result PitchError Pitch
firstBelow thePitchClass startPitch =
    let
        maybePitch =
            allForPitchClass thePitchClass
                |> List.filter
                    (\thePitch ->
                        semitones thePitch < semitones startPitch
                    )
                |> List.sortBy semitones
                |> List.reverse
                |> List.head
    in
    case maybePitch of
        Nothing ->
            Err ValidPitchNotFound

        Just thePitch ->
            Ok thePitch


firstAbove : PitchClass -> Pitch -> Result PitchError Pitch
firstAbove thePitchClass startPitch =
    let
        maybePitch =
            allForPitchClass thePitchClass
                |> List.filter
                    (\thePitch ->
                        semitones thePitch > semitones startPitch
                    )
                |> List.sortBy semitones
                |> List.head
    in
    case maybePitch of
        Nothing ->
            Err ValidPitchNotFound

        Just thePitch ->
            Ok thePitch


errorToString :
    PitchError
    -> String
errorToString error =
    case error of
        InvalidOctave err ->
            Octave.errorToString err

        SemitonesOutOfRange semis ->
            "Pitch semitone count"
                ++ String.fromInt semis
                ++ " is not between the legal range of "
                ++ String.fromInt semitonesLowerLimit
                ++ " and "
                ++ String.fromInt semitonesUpperLimit
                ++ "."

        ValidPitchNotFound ->
            "A valid pitch could not be constructed."

        InternalError ->
            "Something went wrong internally."


transposeUp :
    Interval
    -> Pitch
    -> Result PitchError Pitch
transposeUp =
    transpose PitchClass.transposeUp (+)


transposeDown :
    Interval
    -> Pitch
    -> Result PitchError Pitch
transposeDown =
    transpose PitchClass.transposeDown (-)


transpose :
    (Interval -> PitchClass -> PitchClass)
    -> (Int -> Int -> Int)
    -> Interval
    -> Pitch
    -> Result PitchError Pitch
transpose trans addIntervalSemitones interval p =
    let
        transposedPitchClass =
            pitchClass p
                |> trans interval

        targetOctaveSemitones =
            addIntervalSemitones
                (semitones p)
                (Interval.semitones interval)
                - PitchClass.semitones transposedPitchClass

        numberOfOctaves =
            targetOctaveSemitones // 12

        remainder =
            targetOctaveSemitones |> remainderBy 12
    in
    if remainder == 0 then
        Octave.octave numberOfOctaves
            |> Result.map (\o -> Pitch transposedPitchClass o)
            |> Result.mapError InvalidOctave

    else
        Err InternalError
