module Libs.Ratio exposing
    ( Rational
    , over, fromInt
    , eq, ne, gt, lt, ge, le, max, min, compare
    , add, subtract, multiply, multiplyByInt
    , divide, divideByInt, divideIntBy, negate
    , isZero, isInfinite, round, floor, ceiling, truncate
    , numerator, denominator, split
    , gcd, invert, toFloat
    )

{-| A simple module providing a ratio type for rational numbers


# Types

@docs Rational


# Construction

@docs over, fromInt


# Comparison

@docs eq, ne, gt, lt, ge, le, max, min, compare


# Mathematics

@docs add, subtract, multiply, multiplyByInt
@docs divide, divideByInt, divideIntBy, negate
@docs isZero, isInfinite, round, floor, ceiling, truncate


# Elimination

@docs numerator, denominator, split


# Utils

@docs gcd, invert, toFloat

-}

import Basics exposing (..)


{-| "Arbitrary" (up to `max_int` size) precision fractional numbers. Think of
it as the length of a rigid bar that you've constructed from a bunch of
initial bars of the same fixed length
by the operations of gluing bars together and shrinking a
given bar so that an integer number of copies of it glues together to
make another given bar.
-}
type Rational
    = Rational Int Int


{-| The biggest number that divides both arguments (the greatest common divisor).
-}
gcd : Int -> Int -> Int
gcd a b =
    if b == 0 then
        a

    else
        gcd b (a |> modBy b)



{- Normalisation of rationals with negative denominators

   Rational 1 (-3) becomes Rational (-1) 3

   Rational (-1) (-3) becomes Rational 1 3
-}


normalize (Rational p q) =
    let
        k =
            gcd p q
                * (if q < 0 then
                    -1

                   else
                    1
                  )
    in
    Rational (p // k) (q // k)



{- Add or subtract two rationals :-
   f can be (+) or (-)
-}


addsub : (Int -> Int -> Int) -> Rational -> Rational -> Rational
addsub f (Rational a b) (Rational c d) =
    normalize (Rational (f (a * d) (b * c)) (b * d))


{-| Addition. It's like gluing together two bars of the given lengths.
-}
add : Rational -> Rational -> Rational
add =
    addsub (+)


{-| subtraction. Is it like ungluing two bars of the given lengths?
-}
subtract : Rational -> Rational -> Rational
subtract =
    addsub (-)


{-| Mulitplication. `mulitply x (c / d)` is the length of the bar that you'd get
if you glued `c` copies of a bar of length `x` end-to-end and then shrunk it
down enough so that `d` copies of the shrunken bar would fit in the big
glued bar.
-}
multiply : Rational -> Rational -> Rational
multiply (Rational a b) (Rational c d) =
    normalize (Rational (a * c) (b * d))


{-| Multiply a rational by an Int
-}
multiplyByInt : Rational -> Int -> Rational
multiplyByInt (Rational a b) i =
    normalize (Rational (a * i) b)


{-| Divide two rationals
-}
divide : Rational -> Rational -> Rational
divide r (Rational c d) =
    multiply r (Rational d c)


{-| Divide a rational by an Int
-}
divideByInt : Rational -> Int -> Rational
divideByInt r i =
    normalize (divide r (fromInt i))


{-| Divide an Int by a rational
-}
divideIntBy : Int -> Rational -> Rational
divideIntBy i r =
    normalize (divide (fromInt i) r)



{- This implementation gives the wrong precedence
   divideByInt r i =
     normalize (multiplyByInt (invert r) i)
-}


{-| multiplication by `-1`.
-}
negate : Rational -> Rational
negate (Rational a b) =
    Rational -a b


{-| invert the rational. r becomes 1/r.
-}
invert : Rational -> Rational
invert (Rational a b) =
    normalize (Rational b a)


{-| `over x y` is like `x / y`.
-}
over : Int -> Int -> Rational
over x y =
    if y < 0 then
        normalize (Rational -x -y)

    else
        normalize (Rational x y)


{-| `fromInt x = over x 1`
-}
fromInt : Int -> Rational
fromInt x =
    over x 1


{-| -}
numerator : Rational -> Int
numerator (Rational a _) =
    a


{-| -}
denominator : Rational -> Int
denominator (Rational _ b) =
    b


{-| `split x = (numerator x, denominator x)`
-}
split : Rational -> ( Int, Int )
split (Rational a b) =
    ( a, b )


{-| -}
toFloat : Rational -> Float
toFloat (Rational a b) =
    Basics.toFloat a / Basics.toFloat b


{-| -}
eq : Rational -> Rational -> Bool
eq a b =
    rel (==) a b


{-| -}
ne : Rational -> Rational -> Bool
ne a b =
    rel (/=) a b


{-| -}
gt : Rational -> Rational -> Bool
gt a b =
    rel (>) a b


{-| -}
lt : Rational -> Rational -> Bool
lt a b =
    rel (<) a b


{-| -}
ge : Rational -> Rational -> Bool
ge a b =
    rel (>=) a b


{-| -}
le : Rational -> Rational -> Bool
le a b =
    rel (<=) a b


{-| -}
compare : Rational -> Rational -> Order
compare a b =
    Basics.compare (toFloat a) (toFloat b)


{-| -}
max : Rational -> Rational -> Rational
max a b =
    if gt a b then
        a

    else
        b


{-| -}
min : Rational -> Rational -> Rational
min a b =
    if lt a b then
        a

    else
        b


{-| -}
isZero : Rational -> Bool
isZero r =
    0 == numerator r


{-| -}
isInfinite : Rational -> Bool
isInfinite r =
    0 == denominator r


{-| -}
round : Rational -> Int
round =
    toFloat >> Basics.round


{-| -}
floor : Rational -> Int
floor =
    toFloat >> Basics.floor


{-| -}
ceiling : Rational -> Int
ceiling =
    toFloat >> Basics.ceiling


{-| -}
truncate : Rational -> Int
truncate =
    toFloat >> Basics.truncate


rel : (Int -> Int -> Bool) -> Rational -> Rational -> Bool
rel relop a b =
    relop (numerator a * denominator b) (numerator b * denominator a)
