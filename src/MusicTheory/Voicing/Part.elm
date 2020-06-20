module MusicTheory.Voicing.Part exposing
    ( CompareParts
    , commonTones
    , compareParts
    , semitoneDistance
    , usesContraryMotion
    )

import MusicTheory.Pitch as Pitch


commonTones : Pitch.Pitch -> Pitch.Pitch -> Int
commonTones a b =
    if a == b then
        1

    else
        0


semitoneDistance : Pitch.Pitch -> Pitch.Pitch -> Int
semitoneDistance a b =
    abs (Pitch.semitones a - Pitch.semitones b)


type alias CompareParts =
    { a :
        { from : Pitch.Pitch
        , to : Pitch.Pitch
        }
    , b :
        { from : Pitch.Pitch
        , to : Pitch.Pitch
        }
    }


compareParts :
    voicing
    -> voicing
    -> (voicing -> Pitch.Pitch)
    -> (voicing -> Pitch.Pitch)
    -> CompareParts
compareParts voicingA voicingB getPartA getPartB =
    { a =
        { from = getPartA voicingA
        , to = getPartA voicingB
        }
    , b =
        { from = getPartB voicingA
        , to = getPartB voicingB
        }
    }


usesContraryMotion : CompareParts -> Bool
usesContraryMotion { a, b } =
    Maybe.map2
        (/=)
        (movesUpward a.from a.to)
        (movesUpward b.from b.to)
        |> Maybe.withDefault False


movesUpward : Pitch.Pitch -> Pitch.Pitch -> Maybe Bool
movesUpward a b =
    if (Pitch.semitones a - Pitch.semitones b) == 0 then
        Nothing

    else if (Pitch.semitones a - Pitch.semitones b) > 0 then
        Just False

    else
        Just True
